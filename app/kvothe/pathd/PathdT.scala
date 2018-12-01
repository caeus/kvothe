package kvothe.pathd

import scala.language.higherKinds

import cats.{FlatMap, Functor}
import syntax._

case class PathdT[F[_], A](value: F[Pathd[A]]) {
  def map[B](f: A => B)(implicit functor: Functor[F]): PathdT[F, B] = {
    PathdT(functor.map(value)(_.map(f)))
  }
  def flatMap[B](f: A => PathdT[F,B])(implicit flatMap: FlatMap[F]): PathdT[F, B]={
    flatMapF(a=>f(a).value)
  }

  def flatMapF[B](f: A => F[Pathd[B]])(implicit flatMap: FlatMap[F]): PathdT[F, B] = {
    PathdT(flatMap.flatMap(value) {
      pathdA =>
        flatMap.map(pathdA.map(f).swap)(_.flatMap(identity))
    })
  }
}
