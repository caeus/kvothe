package kvothe.rost

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import cats._
import cats.implicits._
import kvothe.utility.vine.{VineT, Vine}

trait Branch[F[_], In, Format] {

  def compile(segment:String, req: NormalReq): Option[Try[Thunk[F, In, Format]]]

}

object Branch {

  def apply[F[_] : Monad, In, Fragment, Query,
  Body, Out, EOut, Format](
                            fragCompiler: Pipe[String, Option[Fragment]],
                            qCompiler: Pipe[Seq[(String, String)], Try[Query]],
                            bCompiler: Pipe[NormalBody, Try[Body]],
                            resolver: Command[In, Fragment, Query, Body] => F[Out],
                            collector: Out => Vine[EOut],
                            delegate: Grower[F, EOut, Format]
                          ): Branch[F, In, Format] =

    BranchImpl(
      fragCompiler: Pipe[String, Option[Fragment]],
      qCompiler: Pipe[Seq[(String, String)], Try[Query]],
      bCompiler: Pipe[NormalBody, Try[Body]],
      resolver: Command[In, Fragment, Query, Body] => F[Out],
      collector: Out => Vine[EOut],
      delegate: Grower[F, EOut, Format]
    )

  private[rost] case class BranchImpl[F[_] : Monad, In, Segment, Query,
  Body, Out, EOut, Format](
                            segMatcher: Pipe[String, Option[Segment]],
                            qCompiler: Pipe[Seq[(String, String)], Try[Query]],
                            bCompiler: Pipe[NormalBody, Try[Body]],
                            resolver: Command[In, Segment, Query, Body] => F[Out],
                            collector: Out => Vine[EOut],
                            delegate: Grower[F, EOut, Format]
                          ) extends Branch[F, In, Format] {


    private def typedReq(
                          req: NormalReq
                        ): Try[Request[Query, Body]] =
      for {
        query <- qCompiler(req.query)
        body <- bCompiler(req.body)
      } yield req.copy(query = query, body = body)


    def compile(segment:String, req: NormalReq): Option[Try[Thunk[F, In, Format]]] =
      segMatcher(segment).map {
        matched =>
          for {
            typedReq <- typedReq(req)
            next <- req.next.map(Success(_))
              .getOrElse(Failure(new Exception("aljhalskjd")))
            nextThunk <- delegate.compile(next)
          } yield Thunk.branch(MatchedSegment(segment,matched), resolver, typedReq, nextThunk, collector)
      }
  }

}

trait Thunk[F[_], In, Format] {

  def run(in: VineT[F, In]): F[Format]

}

object Thunk {
  def leaf[F[_],In,Format](format: Pipe[Vine[In], Format])(implicit F:Monad[F]): Thunk[F,In,Format] =
    (in: VineT[F, In]) => in.collapse.map(format.apply)


  private[rost] class BranchThunkImpl[F[_] : Monad, In, Segment,
  Query, Body, Out, EOut, Format](
                                   segment: MatchedSegment[Segment],
                                   resolver: Command[In, Segment, Query, Body] => F[Out],
                                   req: Request[Query, Body],
                                   next: Thunk[F, EOut, Format],
                                   collector: Out => Vine[EOut]
                                 ) extends Thunk[F, In, Format] {
    override def run(in: VineT[F, In]): F[Format] = {
      next.run(in.grow(segment.raw) {
        in =>
          resolver(Command(in,segment, req))
      }(collector))
    }
  }

  def branch[F[_] : Monad, In, Segment,
  Query, Body, Out, EOut, Format](segment: MatchedSegment[Segment],
                                  resolver: Command[In, Segment, Query, Body] => F[Out],
                                  req: Request[Query, Body],
                                  next: Thunk[F, EOut, Format],
                                  collector: Out => Vine[EOut]
                                 ): Thunk[F, In, Format] = new BranchThunkImpl(
    segment,
    resolver: Command[In, Segment, Query, Body] => F[Out],
    req: Request[Query, Body],
    next: Thunk[F, EOut, Format],
    collector)

}




