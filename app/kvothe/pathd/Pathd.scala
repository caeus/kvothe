package kvothe.pathd

import scala.language.higherKinds

import cats.{Apply, Functor}
import cats.implicits._
import cats._

case class Pathd[+T](path: Seq[String], value: T) {

  def map[R](f: T => R): Pathd[R] = Pathd(path, f(value))

  def flatMap[R](f: T => Pathd[R]): Pathd[R] = {
    val pathd = f(value)
    Pathd(path ++ pathd.path, pathd.value)
  }


}

object a{


  Pathd(Seq("alskj"),Some(34):Option[Int]).swap[Option,Int]
}

