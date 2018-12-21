package io.sower

import scala.language.{higherKinds, implicitConversions}
import scala.util.{Success, Try}

import cats.Id
import cats._
import cats.implicits._
import cats.data._


case class Packer[In, Out](f:Kleisli[Id,In,Out]) {
  def value: Kleisli[Id, In, Out] = f
}

trait Unpacker[In, Out] {
  value.flatMapF(Success(_))
  def value: Kleisli[Try, In, Out]
}


trait Pipe[In, Out] extends Any {
  def apply(in: In): Out

  final def map[NewOut](f: Out => NewOut): Pipe[In, NewOut] = Pipe {
    in =>
      f(apply(in))
  }

  final def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out] = Pipe {
    newIn =>
      apply(f(newIn))
  }
}

object Pipe {

  implicit def apply[In, Out](f: In => Out): Pipe[In, Out] = new PipeImpl(f)

  class PipeImpl[In, Out](val func: In => Out) extends AnyVal with Pipe[In, Out] {
    override def apply(in: In): Out = func(in)
  }

  def id[A]: Pipe[A, A] = Pipe[A, A](identity)

  def tryPure[A]: Pipe[A, Try[A]] = Pipe[A, Try[A]](Success.apply)
}