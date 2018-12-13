package edu.caeus.herbivicus.sower

import scala.language.implicitConversions
import scala.util.{Success, Try}

trait Pipe[In, Out] extends Any {
  def apply(in: In): Out

  final def map[NewOut](f: Out => NewOut): Pipe[In, NewOut]= Pipe{
    in=>
      f(apply(in))
  }

  final def contramap[NewIn](f: NewIn => In): Pipe[NewIn, Out]=Pipe{
    newIn=>
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