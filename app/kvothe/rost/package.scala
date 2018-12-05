package kvothe

import cats.Monad
import kvothe.utility.tson.Tson

import scala.language.{higherKinds, implicitConversions}
import scala.util.{Success, Try}

package object rost {


  type NormalBody = String
  type NormalQuery = Seq[(String, String)]


  type NormalCmd[In] = Command[In, String, NormalQuery, NormalBody]
  type NormalReq = Request[NormalQuery, NormalBody]

  implicit class StringOps(val value: String) extends AnyVal {
    def seg: SegmentMatcher[String] = Pipe[String, Option[String]](s => Some(s).filter { x =>
      println (x->value->"alskdjlaksjdlkas")
      x == value
    })
  }

  case class MatchedSegment[Segment](raw: String, value: Segment)

  type SegmentMatcher[Out] = Pipe[String, Option[Out]]
  type QueryParser[Out] = Pipe[Seq[(String, String)], Try[Out]]
  type BodyParser[Out] = Pipe[NormalBody, Try[Out]]

  trait Pipe[-In, +Out] extends Any {
    def apply(in: In): Out

    def map[NewOut](f: Out => NewOut): Pipe[In, NewOut]

    def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out]
  }

  object Pipe {

    implicit def apply[In, Out](f: In => Out): Pipe[In, Out] = new PipeImpl(f)

    class PipeImpl[-In, +Out](val func: In => Out) extends AnyVal with Pipe[In, Out] {
      override def apply(in: In): Out = func(in)

      override def map[NewOut](f: Out => NewOut): Pipe[In, NewOut] = Pipe(func.andThen(f))

      override def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out] = Pipe(f.andThen(func))
    }


    def id[A]: Pipe[A, A] = Pipe[A, A](identity)

    def tryPure[A]: Pipe[A, Try[A]] = Pipe[A, Try[A]](Success.apply)
  }


  trait BranchSealer[F[_], In, Out, Format] {
    def into(schema: Tree[F, Out, Format]): Branch[F, In, Format]
  }


  case class BranchBuilder[F[_] : Monad, In, Segment, Query, Body, Format]
  (
    segmentMatcher: SegmentMatcher[Segment],
    queryParser: QueryParser[Query],
    bodyParser: BodyParser[Body]
  ) {
    type RawSealer[Out] = BranchSealer[F, In, Out, Format]
    type RawCmd = Command[In, Segment, Query, Body]

    def redge[Out, EOut](resolver: RawCmd => F[Out], collctor: Out => Tson[EOut]): BranchSealer[F, In, EOut, Format] =
      (schema: Tree[F, EOut, Format]) => {
        Branch(segmentMatcher, queryParser, bodyParser, resolver, collctor, schema)
      }

    def id[Out](resolver: RawCmd => F[Out]): RawSealer[Out] = redge[Out, Out](resolver, Tson.pure)

    def option[Out](resolver: RawCmd => F[Option[Out]]): RawSealer[Out] =
      redge[Option[Out], Out](resolver, Tson.option)

    def map[Out](resolver: RawCmd => F[Map[String, Out]]): RawSealer[Out] =
      redge[Map[String, Out], Out](resolver, Tson.map)

    def seq[Out](resolver: RawCmd => F[Seq[Out]]): RawSealer[Out] =
      redge[Seq[Out], Out](resolver, Tson.seq)

    def query[NewQuery](as: QueryParser[NewQuery]): BranchBuilder[F, In, Segment, NewQuery, Body, Format] =
      copy(queryParser = as)

    def body[NewBody](as: BodyParser[NewBody]): BranchBuilder[F, In, Segment, Query, NewBody, Format] =
      copy(bodyParser = as)
  }

  class BranchSelectorBuilder[F[_] : Monad, In, Format] {
    def path[Segment](regex: SegmentMatcher[Segment]): BranchBuilder[F, In, Segment, NormalQuery, String, Format] = {
      BranchBuilder[F, In, Segment, NormalQuery, String, Format](regex, Pipe.tryPure, Pipe.tryPure)
    }
  }


}
