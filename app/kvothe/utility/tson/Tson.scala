package kvothe.utility.tson

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.unchecked.uncheckedVariance
import scala.language.reflectiveCalls

sealed abstract class Tson[+A] {

  def map[B](f: A => B): Tson[B]

  def flatMap[B](f: A => Tson[B]): Tson[B]

}

class EffTsonOps[F[_], A](val tson: Tson[F[A]]) extends AnyVal {
  def foldRec(implicit m: Monad[F]): F[Tson[A]] = Tson.fold(tson)
}

object Tson {


  case class ValOf[+A](value: A) extends Tson[A] {

    override def map[B](f: A => B): Tson[B] = ValOf(f(value))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = {
      f(value)
    }
  }

  case class ArrayOf[+A](value: Seq[Tson[A]]) extends Tson[A] {
    override def map[B](f: A => B): Tson[B] = ArrayOf(value.map(_.map(f)))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = {
      ArrayOf(value.map { seq =>
        seq.flatMap(f)
      })
    }


  }

  case class DictOf[F[_], +A](value: Map[String, Tson[A]]) extends Tson[A] {
    private type StringMap[X] = Map[String, X]

    override def map[B](f: A => B): Tson[B] = DictOf(value.mapValues(_.map(f)))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = DictOf(value.mapValues(_.flatMap(f)))

  }

  case object Empty extends Tson[Nothing] {
    override def map[B](f: Nothing => B): Tson[B] = this

    override def flatMap[B](f: Nothing => Tson[B]): Tson[B] = this
  }

  implicit def effTsonOps[F[_], A](tson: Tson[F[A]]): EffTsonOps[F, A] = new EffTsonOps[F, A](tson)

  private[tson] def fold[F[_] : Monad, A](tson: Tson[F[A]]): F[Tson[A]] = {
    type StringMap[X] = Map[String, X]

    def recFold(tson: Tson[F[A]]): F[Tson[A]] = {
      tson match {
        case ValOf(fa) => fa.map(ValOf(_))
        case ArrayOf(seq) =>
          seq.map(recFold).toList.sequence.map(ArrayOf(_))
        case DictOf(map) =>
          map.mapValues(recFold).map {
            case (path, value) => value.map(path -> _)
          }.toList.sequence.map(_.toMap).map(DictOf(_))
        case empty@Empty =>
          Monad[F].pure(empty)
      }
    }
    recFold(tson)
  }

}
