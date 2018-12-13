package edu.caeus.herbivicus.sower

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import cats._
import cats.implicits._
import edu.caeus.herbivicus.sower.data.{BranchReq, Request}
import edu.caeus.herbivicus.vine.{Vine, VineT}
import monix.eval.Task


abstract class Sower[F[_] : Monad, Format] {

  type Sync[X] = Try[X]
  type QueryParams = Seq[(String, String)]
  type RequestBody = Format
  type BranchSelector[Segment] = Pipe[Option[String], Option[Segment]]
  type QueryParser[Query] = Pipe[QueryParams, Sync[Query]]
  type BodyParser[Body] = Pipe[RequestBody, Sync[Body]]
  type NormalReq = Request[RequestBody]
  type Writer[X] = Pipe[X, Format]

  private[sower] def monadOfF: Monad[F] = Monad[F]

  implicit def vineWriter[X](implicit xWriter: Writer[X]): Writer[Vine[X]]

  final def normalQueryParser: QueryParser[QueryParams] = Pipe[QueryParams, Sync[QueryParams]](params => Try(params))

  final def normalBodyParser: BodyParser[RequestBody] = Pipe[RequestBody, Sync[RequestBody]](params => Try(params))

  sealed trait Sprout[In] {
    def compile(request: NormalReq): Sync[Program[In]]

    final def narrow[NewIn](func: NewIn => Vine[In]): Sprout[NewIn] = {
      NarrowedSprout(func, this)
    }

    final def ? : Sprout[Option[In]] = narrow(Vine.option)

    final def <> : Sprout[Seq[In]] = narrow(Vine.seq)

    final def <:> : Sprout[Map[String, In]] = narrow(Vine.map)
  }

  private case class NarrowedSprout[NewIn, In](
    narrower: NewIn => Vine[In],
    underlying: Sprout[In]
  ) extends Sprout[NewIn] {
    override def compile(request: NormalReq): Sync[Program[NewIn]] = {
      underlying.compile(request).map {
        program =>
          program.narrow(narrower)
      }
    }
  }

  sealed trait Program[In] {
    def narrow[NewIn](narrower: NewIn => Vine[In]): Program[NewIn] = NarrowedProgram(narrower, this)

    def run(in: VineT[F, In]): F[Format]
  }

  private case class NarrowedProgram[NewIn, In](
    narrower: NewIn => Vine[In],
    underlying: Program[In]
  ) extends Program[NewIn] {
    override def run(in: VineT[F, NewIn]): F[Format] = {
      underlying.run(in.subflatMap(narrower))
    }
  }


  private case class BranchProgram[In, Segment, Query, Body, Out](
    rawSegment:String,
    segment: Segment,
    request: BranchReq[Segment, Query, Body],
    resolver: BranchReq[Segment, Query, Body] => In => F[Out],
    next: Program[Out]
  ) extends Program[In] {
    override def run(in: VineT[F, In]): F[Format] = {
      next.run(in.semiflatMap {
        in =>
          resolver(request)(in)
      }.growBranch(rawSegment))
    }
  }

  private case class LeafProgram[In](inWriter: Writer[In]) extends Program[In] {
    override def run(in: VineT[F, In]): F[Format] = {
      in.value.asInstanceOf[Task[Vine[In]]].map{println}.runAsync(monix.execution.Scheduler.Implicits.global)

      in.value.map(vineWriter(inWriter).apply)
    }
  }

  def ramification[In](branches: Seq[Branch[In]]): Sprout[In] =
    Ramification(branches)

  private case class Ramification[In](branches: Seq[Branch[In]]) extends Sprout[In] {

    override def compile(request: NormalReq): Sync[Program[In]] = {
      request.currentSegment.map{
        rawSegment=>
          branches.iterator.map(_.compile(rawSegment,request)).dropWhile(_.isEmpty)
            .map(_.get).toIterable
            .headOption.getOrElse(Failure(new Exception(
            s"""No branch matched $request"""
          )))
      }.getOrElse(Failure(new Exception("End of request, it was not matched")))

    }
  }
  def leaf[In](implicit writer: Pipe[In,Format]): Sprout[In] = Leaf(writer)

  private case class Leaf[In](writer: Writer[In]) extends Sprout[In] {
    override def compile(request: NormalReq): Sync[Program[In]] =
      Success(LeafProgram(writer))
  }

  def branch[In, Segment, Query, Body, Out](
    selector: BranchSelector[Segment],
    queryParser: QueryParser[Query],
    bodyParser: BodyParser[Body],
    resolver: BranchReq[Segment, Query, Body] => In => F[Out],
    into: Sprout[Out]
  ): Branch[In] = {
    BranchImpl(selector: BranchSelector[Segment],
      queryParser: QueryParser[Query],
      bodyParser: BodyParser[Body],
      resolver: BranchReq[Segment, Query, Body] => In => F[Out],
      into: Sprout[Out])
  }

  trait Branch[In] {
    def compile(segment:String, request: NormalReq): Option[Sync[Program[In]]]
  }


  private case class BranchImpl[In, Segment, Query, Body, Out](
    selector: BranchSelector[Segment],
    queryParser: QueryParser[Query],
    bodyParser: BodyParser[Body],
    resolver: BranchReq[Segment, Query, Body] => In => F[Out],
    into: Sprout[Out]
  ) extends Branch[In] {
    override def compile(rawSegment:String, request: NormalReq): Option[Sync[Program[In]]] = {
      selector(request.currentSegment).map {
        segment =>
          for {
            query <- queryParser(request.query)
            body <- bodyParser(request.body)
            next <- request.next.map(Success(_))
              .getOrElse(Failure(new Exception("Fix"))).flatMap {
              nextRequest =>
                into.compile(nextRequest)
            }
          } yield BranchProgram(rawSegment,segment, BranchReq(segment,query,body), resolver, next)
      }
    }
  }

}
class ConcreteSower[F[_] : Monad, Format](vineFolder: Vine.PartialFolder[Format]) extends Sower[F, Format] {
  override implicit def vineWriter[X](implicit xWriter: Writer[X]): Writer[Vine[X]] = Pipe[Vine[X], Format] {
    vineOfX => vineOfX.foldWith(vineFolder(xWriter.apply))
  }
}

object Sower {


  private[sower] class ApplyPartiallyApplied[F[_]] {
    def apply[Format](vineFolder: Vine.PartialFolder[Format])
      (implicit F: Monad[F]): Sower[F, Format] =
      new ConcreteSower[F, Format](vineFolder)
  }
  def apply[F[_]] = new ApplyPartiallyApplied[F]
}





