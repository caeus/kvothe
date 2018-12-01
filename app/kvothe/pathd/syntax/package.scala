package kvothe.pathd

import scala.language.higherKinds

import cats.Functor
import cats.syntax.functor._

package object syntax {
  implicit class StringSeqPathOPs(val path: Seq[String]) {
    def pathd[A](value: A) = Pathd(path, value)
  }

  implicit class PathdFOps[F[_] : Functor, T](val pathd: Pathd[F[T]]) {
    def swap: F[Pathd[T]] = pathd.value.map {
      t =>
        Pathd(pathd.path, t)
    }
  }

  def pathd[T](path:String*)(t:T): Pathd[T] =Pathd(path,t)

}
