package kvothe

import cats.Monad
import kvothe.utility.vine.Vine

import scala.language.{higherKinds, implicitConversions}
import scala.util.{Success, Try}

package object rost {


  type NormalBody = String
  type NormalQuery = Seq[(String, String)]


  type NormalCmd[In] = Command[In, String, NormalQuery, NormalBody]
  type NormalReq = Request[NormalQuery, NormalBody]

  implicit class StringOps(val value: String) extends AnyVal {
    def seg: SegmentMatcher[String] = Pipe[String, Option[String]](s => Some(s).filter { x =>
      x == value
    })
  }


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





}
