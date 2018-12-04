package kvothe.rost

import scala.language.higherKinds
import scala.util.Try

import cats._
import cats.implicits._

trait Property[F[_], In, Format] {

  def compile(req: NormalReq): Option[Try[CompiledAction[F, In, Format]]]

}

object Property {
  def id[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody , Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format]
  ): Property[F, In, Format] = IdPropertyImpl(
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody , Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format]
  )

  private[rost] case class OptPropertyImpl[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody , Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Option[Out]],
    delegate: Schema[F, Out, Format]
  ) extends Property[F, In, Format] {


    private def typedReq(
      fragment: Fragment,
      req: NormalReq
    ): Try[Req[Fragment, Query, Body]] = {
      qCompiler(req.query).flatMap {
        query =>
          bCompiler(req.body).map {
            body =>
              Req(fragment, query, body)
          }
      }
    }

    def compile(req: NormalReq): Option[Try[CompiledAction[F, In, Format]]] = {
      fragCompiler(req.fragment).map {
        fragment =>
          typedReq(fragment, req).flatMap {
            req =>
              delegate.compile(req.next).map {
                nextExecutor =>
                  CompiledAction.nested(resolver, req, nextExecutor)
              }
          }
      }
    }
  }

  private[rost] case class IdPropertyImpl[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody , Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format]
  ) extends Property[F, In, Format] {


    private def typedReq(
      fragment: Fragment,
      req: NormalReq
    ): Try[Req[Fragment, Query, Body]] = {
      qCompiler(req.query).flatMap {
        query =>
          bCompiler(req.body).map {
            body =>
              Req(fragment, query, body)
          }
      }
    }

    def compile(req: NormalReq): Option[Try[CompiledAction[F, In, Format]]] = {
      fragCompiler(req.fragment).map {
        fragment =>
          typedReq(fragment, req).flatMap {
            req =>
              delegate.compile(req.next).map {
                nextExecutor =>
                  CompiledAction.nested(resolver, req, nextExecutor)
              }
          }
      }
    }
  }
}

trait CompiledAction[F[_], In, Format] {

  def run(in: In): F[Format]

}

object CompiledAction {
  private[rost] case class NestedCompiledActionImpl[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, Out, Format]
  ) extends CompiledAction[F, In, Format] {
    override def run(in: In): F[Format] = {
      val ctx = req.build(in)
      resolver(ctx).flatMap(next.run)
    }
  }

  //FIXME for map, and array and optional
  def nested[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, Out, Format]
  ): CompiledAction[F, In, Format] = NestedCompiledActionImpl(
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, Out, Format]
  )
}




