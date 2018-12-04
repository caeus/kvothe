package kvothe.rost

import scala.language.higherKinds
import scala.util.Try

import cats._
import kvothe.utility.tson.TCursor

trait Property[F[_], In, Format] {

  def compile(req: NormalReq): Option[Try[CompiledAction[F, In, Format]]]

}

object Property {

  def apply[F[_],In,Segment,Query,Body,Out,EOut,Format](
    fragCompiler: Pipe[String, Option[Segment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody, Try[Body]],
    resolver: Action[In, Segment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format],
    req:String=>TCursor[F,In]=>(In=>F[Out])=>TCursor[F,EOut])={

  }

  def id[F[_] : Monad, In, Fragment, Query, Body, Out, Format](
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody, Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format]
  ): Property[F, In, Format] = PropertyImpl(
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody, Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    delegate: Schema[F, Out, Format]
  )



  private[rost] case class PropertyImpl[F[_] : Monad, In, Fragment, Query, Body, Out, EOut, Format](
    fragCompiler: Pipe[String, Option[Fragment]],
    qCompiler: Pipe[Seq[(String, String)], Try[Query]],
    bCompiler: Pipe[NormalBody, Try[Body]],
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    downer:String=>TCursor[F,In]=>(In=>F[Out])=>TCursor[F,EOut],
    delegate: Schema[F, EOut, Format]
  ) extends Property[F, In, Format] {


    private def typedReq(
      fragment: Fragment,
      req: NormalReq
    ): Try[Req[Fragment, Query, Body]] = {
      qCompiler(req.query).flatMap {
        query =>
          bCompiler(req.body).map {
            body =>
              Req(fragment, req.segment, query, body)
          }
      }
    }

    def compile(req: NormalReq): Option[Try[CompiledAction[F, In, Format]]] = {
      fragCompiler(req.segment).map {
        fragment =>
          typedReq(fragment, req).flatMap {
            req =>
              delegate.compile(req.next).map {
                nextExecutor =>
                  CompiledAction.nested(resolver, req, nextExecutor, downer)
              }
          }
      }
    }
  }
}

trait CompiledAction[F[_], In, Format] {

  def run(in: TCursor[F, In]): F[Format]

}

object CompiledAction {


  private[rost] class NestedCompiledActionImpl[F[_] : Monad, In, Fragment, Query, Body, Out, EOut, Format](
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, EOut, Format],
    piece: String=> TCursor[F, In] => ((In => F[Out]) => TCursor[F, EOut])
  ) extends CompiledAction[F, In, Format] {
    override def run(in: TCursor[F, In]): F[Format] = {
      next.run(piece(req.rawSegment)(in) { in =>
        resolver(req.build(in))
      })
    }
  }

  def nested[F[_] : Monad, In, Fragment, Query, Body, Out, EOut, Format](
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, EOut, Format],
    piece: String=>TCursor[F, In] => ((In => F[Out]) => TCursor[F, EOut])
  ):CompiledAction[F,In,Format] = new NestedCompiledActionImpl(
    resolver: Action[In, Fragment, Query, Body] => F[Out],
    req: Req[Fragment, Query, Body],
    next: CompiledAction[F, EOut, Format],
    piece: String=>TCursor[F, In] => ((In => F[Out]) => TCursor[F, EOut]))

}




