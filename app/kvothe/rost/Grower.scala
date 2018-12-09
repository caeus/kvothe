package kvothe.rost

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import cats._
import cats.implicits._
import kvothe.utility.vine.{VineT, Vine}


abstract class Grower[F[_] : Monad, Format] {


  type Sync[X] = Try[X]
  type QueryParams = Seq[(String, String)]
  type RequestBody = Format
  type BranchSelector[Segment] = Pipe[Option[String], Option[Segment]]
  type QueryParser[Query] = Pipe[QueryParams, Sync[Query]]
  type BodyParser[Body] = Pipe[RequestBody, Sync[Body]]
  type NormalReq = Request[QueryParams, RequestBody]
  type Writer[X] = Pipe[X, Format]

  implicit def vineWriter[X](implicit xWriter: Writer[X]): Writer[Vine[X]]

  sealed trait Sprout[In] {
    def compile(request: NormalReq): Sync[Program[In]]

    final def narrow[NewIn](func: NewIn => Vine[In]): Sprout[NewIn] = {
      NarrowedSprout(func, this)
    }

    final def ? : Sprout[Option[In]] = narrow(Vine.option)

    final def <> : Sprout[Seq[In]] = narrow(Vine.seq)

    final def <:> : Sprout[Map[String, In]] = narrow(Vine.map)
  }

  private case class NarrowedSprout[NewIn, In](narrower: NewIn => Vine[In], underlying: Sprout[In]) extends Sprout[NewIn] {
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

  private case class NarrowedProgram[NewIn, In](narrower: NewIn => Vine[In], underlying: Program[In]) extends Program[NewIn] {
    override def run(in: VineT[F, NewIn]): F[Format] = {
      underlying.run(in.subflatMap(narrower))
    }
  }


  private case class BranchProgram[In, Segment, Query, Body, Out](
                                                                   segment: Segment,
                                                                   request: Request[Query, Body],
                                                                   resolver: Command[In, Segment, Query, Body] => F[Out],
                                                                   next: Program[Out]
                                                                 ) extends Program[In] {
    override def run(in: VineT[F, In]): F[Format] = {
      next.run(in.semiflatMap {
        in =>
          resolver(Command(in, segment, request))
      })
    }
  }

  private case class LeafProgram[In](inWriter: Writer[In]) extends Program[In] {
    override def run(in: VineT[F, In]): F[Format] = {
      in.value.map(vineWriter(inWriter).apply)
    }
  }

  def ramification[In](branches: Seq[Branch[In]]): Sprout[In] =
    Ramification(branches)


  private case class Ramification[In](branches: Seq[Branch[In]]) extends Sprout[In] {


    override def compile(request: NormalReq): Sync[Program[In]] = {
      branches.iterator.map(_.compile(request)).dropWhile(_.isEmpty)
        .map(_.get).toIterable
        .headOption.getOrElse(Failure(new Exception("shit ramitfadafa")))
    }
  }
  def leaf[In](implicit writer: Writer[In]):Sprout[In] = Leaf()

  private case class Leaf[In](implicit writer: Writer[In]) extends Sprout[In] {
    override def compile(request: NormalReq): Sync[Program[In]] =
      Success(LeafProgram(writer))
  }

  def branch[In, Segment, Query, Body, Out](selector: BranchSelector[Segment],
                                            queryParser: QueryParser[Query],
                                            bodyParser: BodyParser[Body],
                                            resolver: Command[In, Segment, Query, Body] => F[Out],
                                            into: Sprout[Out]): Branch[In] = {
    BranchImpl(selector: BranchSelector[Segment],
      queryParser: QueryParser[Query],
      bodyParser: BodyParser[Body],
      resolver: Command[In, Segment, Query, Body] => F[Out],
      into: Sprout[Out])
  }

  trait Branch[In] {
    def compile(request: NormalReq): Option[Sync[Program[In]]]
  }


  private case class BranchImpl[In, Segment, Query, Body, Out](selector: BranchSelector[Segment],
                                                               queryParser: QueryParser[Query],
                                                               bodyParser: BodyParser[Body],
                                                               resolver: Command[In, Segment, Query, Body] => F[Out],
                                                               into: Sprout[Out]) extends Branch[In] {
    override def compile(request: NormalReq): Option[Sync[Program[In]]] = {
      selector(request.currSegment).map {
        segment =>
          for {
            query <- queryParser(request.query)
            body <- bodyParser(request.body)
            next <- request.next.map(Success(_))
              .getOrElse(Failure(new Exception("Fix"))).flatMap {
              nextRequest =>
                into.compile(nextRequest)
            }
          } yield BranchProgram(segment, request.copy(query = query, body = body), resolver, next)
      }
    }
  }

}





