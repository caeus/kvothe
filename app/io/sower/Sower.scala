package io.sower

import scala.language.higherKinds
import scala.util.control.NonFatal

import cats._
import cats.implicits._
import io.vine.{Vine, VineT}


abstract class Sower[F[_], Input, Output](implicit monadError: MonadError[F, Throwable]) {

  type Sync[X] = Either[Throwable, X]
  type SegmentParser[X] = Pipe[Option[String], Option[X]]
  type InputParser[X] = Pipe[Input, F[X]]
  type NormalReq = Request[Input]
  type Writer[X] = Pipe[X, Output]

  private[sower] def monadOfF: MonadError[F, Throwable] = monadError

  implicit def vineWriter[X](implicit xWriter: Writer[X]): Writer[Vine[X]]


  final def normalInputParser: InputParser[Input] = Pipe[Input, F[Input]](monadError.pure)

  sealed trait Sprout[From] {
    def compile(request: NormalReq): F[Program[From]]

    final def narrow[NewIn](func: NewIn => Vine[From]): Sprout[NewIn] = {
      NarrowedSprout(func, this)
    }

    final def optional: Sprout[Option[From]] = narrow(Vine.option)

    final def array: Sprout[Seq[From]] = narrow(Vine.seq)

    final def dictionary: Sprout[Map[String, From]] = narrow(Vine.map)

  }

  private case class NarrowedSprout[NewIn, In](
    narrower: NewIn => Vine[In],
    underlying: Sprout[In]
  ) extends Sprout[NewIn] {
    override def compile(request: NormalReq): F[Program[NewIn]] = {
      underlying.compile(request).map {
        program =>
          program.narrow(narrower)
      }
    }
  }

  sealed trait Program[From] {
    def narrow[NewFrom](narrower: NewFrom => Vine[From]): Program[NewFrom] = NarrowedProgram(narrower, this)

    def run(in: VineT[F, From]): F[Output]
  }

  private case class NarrowedProgram[NewFrom, From](
    narrower: NewFrom => Vine[From],
    underlying: Program[From]
  ) extends Program[NewFrom] {
    override def run(in: VineT[F, NewFrom]): F[Output] = {
      underlying.run(in.subflatMap(narrower))
    }
  }


  private case class BranchProgram[From, Segment, Payload, To](
    segmentName: String,
    request: BranchReq[Segment, Payload],
    resolver: BranchReq[Segment, Payload] => From => F[To],
    next: Program[To]
  ) extends Program[From] {
    override def run(in: VineT[F, From]): F[Output] = {
      next.run(in.semiflatMap {
        in =>
          resolver(request)(in)
      }.growBranch(segmentName))
    }
  }

  private case class LeafProgram[From](inWriter: Writer[From]) extends Program[From] {
    override def run(in: VineT[F, From]): F[Output] = {
      in.value.map { x =>
        vineWriter(inWriter)(x)
      }
    }
  }

  def ramification[From](branches: Seq[Branch[From]]): Sprout[From] =
    Ramification(branches)

  private case class Ramification[From](branches: Seq[Branch[From]]) extends Sprout[From] {

    override def compile(request: NormalReq): F[Program[From]] = {
      request.currentSegment.map {
        rawSegment =>
          branches.iterator.map(_.compile(rawSegment, request)).dropWhile(_.isEmpty)
            .map(_.get).toIterable
            .headOption
            .getOrElse(monadError.raiseError(NoBranchFoundError(request)))
      }.getOrElse(monadError.raiseError(EndOfRouteFound(request)))
    }
  }
  def leaf[From](implicit writer: Pipe[From, Output]): Sprout[From] = Leaf(writer)

  private case class Leaf[From](writer: Writer[From]) extends Sprout[From] {
    override def compile(request: NormalReq): F[Program[From]] =
      monadError.pure(LeafProgram(writer))
  }

  def branch[From, Segment, Payload, To](
    selector: BranchSelector[Segment],
    inputParser: InputParser[Payload],
    resolver: BranchReq[Segment, Payload] => From => F[To],
    into: Sprout[To]
  ): Branch[From] = {
    BranchImpl(selector: BranchSelector[Segment],
      inputParser: InputParser[Payload],
      resolver: BranchReq[Segment, Payload] => From => F[To],
      into: Sprout[To])
  }

  trait Branch[In] {
    def compile(segment: String, request: NormalReq): Option[F[Program[In]]]
  }


  private case class BranchImpl[From, Segment, Payload, To](
    selector: BranchSelector[Segment],
    inputParser: InputParser[Payload],
    resolver: BranchReq[Segment, Payload] => From => F[To],
    into: Sprout[To]
  ) extends Branch[From] {
    override def compile(
      rawSegment: String,
      request: NormalReq
    ): Option[F[Program[From]]] = {

      selector.matchRawSegment(rawSegment).map {
        segment =>
          //I need this intermediate result because compiler sucks
          for {
            segmentPayload <- monadError.product(segment.fold({
              case NonFatal(e) =>
                monadError.raiseError[Segment](ParsingError(s"Error parsing segment: $rawSegment", request, Some(e)))
            }, monadError.pure), inputParser(request.payload).recoverWith {
              case NonFatal(e) =>
                monadError.raiseError(ParsingError(s"Error parsing payload: ${request.payload}-> ${e.getMessage}", request, Some(e)))
            })
            next<- request.next.map(Right(_))
              .getOrElse(Left(EndOfRouteFound(request)))
              .fold(t=>monadError.raiseError[Program[To]](t), into.compile)
          } yield BranchProgram(selector.name, BranchReq(segmentPayload._1, segmentPayload._2), resolver, next)
      }
    }
  }

}
class ConcreteSower[F[_], Input, Output](vineCombiner: Vine.Combiner[Output])
  (implicit F: MonadError[F, Throwable]) extends Sower[F, Input, Output] {
  override implicit def vineWriter[X](implicit xWriter: Writer[X]): Writer[Vine[X]] = Pipe[Vine[X], Output] {
    vineOfX =>
      val value = vineOfX.map(xWriter.apply)
      value.combineWith(vineCombiner)
  }
}

object Sower {


  private[sower] class ApplyPartiallyApplied[F[_], Input] {
    def apply[Output](vineFolder: Vine.Combiner[Output])
      (implicit F: MonadError[F, Throwable]): Sower[F, Input, Output] =
      new ConcreteSower[F, Input, Output](vineFolder)
  }
  def apply[F[_], Input] = new ApplyPartiallyApplied[F, Input]
}





