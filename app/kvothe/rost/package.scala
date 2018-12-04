package kvothe

import scala.language.higherKinds
import scala.util.Try

package object rost {


  type NormalBody = String


  type NormalCtx[In] = Action[In, String, Seq[(String, String)], NormalBody]
  type NormalReq = Req[String, Seq[(String, String)], NormalBody]

  type PathMatcher[+Out] = Pipe[String, Option[Out]]
  type QueryParser[+Out] = Pipe[Seq[(String, String)], Try[Out]]
  type BodyParser[+Out] = Pipe[NormalBody, Try[Out]]

  trait Pipe[-In, +Out] {
    def apply(in: In): Out
    def map[NewOut](f: Out => NewOut): Pipe[In, NewOut]
    def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out]
  }

  object Pipe {
    class PipeImpl[-In, +Out](val func: In => Out) extends AnyVal with Pipe[In, Out] {
      override def apply(in: In): Out = func(in)

      override def map[NewOut](f: Out => NewOut): Pipe[In, NewOut] = Pipe(func.andThen(f))

      override def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out] = Pipe(f.andThen(func))
    }
    def apply[In, Out](f: In => Out): Pipe[In, Out] = new PipeImpl(f)

    def id[A]: Pipe[A, A] = Pipe[A, A](identity)
  }


  trait PropertyBuilder[F[_], In, Out, Format] {
    def into(schema: Schema[F, Out, Format]): Property[F, In, Format]
  }

  class PropertyBuilderImpl[F[_], In, Out, Format] extends PropertyBuilder[F, In, Out, Format] {
    override def into(schema: Schema[F, Out, Format]): Property[F, In, Format] = ???
  }
  trait InputBuilder[F[_], In, Frag, Query, Body, Format] {
    type PropBinder[Out] = PropertyBuilder[F, In, Out, Format]
    type RawCtx = Action[In, Frag, Query, Body]
    def id[Out](resolver: RawCtx => F[Out]): PropBinder[Out]
    def opt[Out](resolver: RawCtx => F[Option[Out]]): PropBinder[Out]
    def dict[Out](resolver: RawCtx => F[Map[String, Out]]): PropBinder[Out]
    def arr[Out](resolver: RawCtx => F[Seq[Out]]): PropBinder[Out]
    def query[NewQuery](as: Query => NewQuery): InputBuilder[F, In, Frag, NewQuery, Body, Format]
    def body[NewBody](as: Body => NewBody): InputBuilder[F, In, Frag, Query, NewBody, Format]
  }

  class InputBuilderImpl[F[_], In, Frag, Query, Body, Format]
  (
    pathMatcher: PathMatcher[Frag],
    queryParser: QueryParser[Query],
    bodyParser: BodyParser[Body]
  ) extends InputBuilder[F, In, Frag, Query, Body, Format] {

    override def id[Out](resolver: RawCtx => F[Out]): PropBinder[Out] = ???

    override def opt[Out](resolver: RawCtx => F[Option[Out]]): PropBinder[Out] = ???

    override def dict[Out](resolver: RawCtx => F[Map[String, Out]]): PropBinder[Out] = ???

    override def arr[Out](resolver: RawCtx => F[Seq[Out]]): PropBinder[Out] = ???

    override def query[NewQuery](as: Query => NewQuery): InputBuilder[F, In, Frag, NewQuery, Body, Format] = ???

    override def body[NewBody](as: Body => NewBody): InputBuilder[F, In, Frag, Query, NewBody, Format] = ???
  }

  trait PathBuilder[F[_], In, Format] {
    def path[Frag](regex: PathMatcher[Frag]): InputBuilder[F, In, Frag, Seq[(String, String)], String, Format]
  }


}
