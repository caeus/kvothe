package kvothe.utility.tson

import cats._
import cats.implicits._

import scala.language.higherKinds


sealed abstract class Tson[+A] {

  def map[B](f: A => B): Tson[B]

  def flatMap[B](f: A => Tson[B]): Tson[B]

  def fold[B](ifEmpty: =>B,
              ifVal: A=>B,
              ifArray:Seq[Tson[A]]=>B,
              ifDict: Map[String,Tson[A]]=>B):B

  def isEmpty:Boolean
  def isVal:Boolean
  def isArray:Boolean
  def isDict:Boolean
}


object Tson {

  def empty[A]: Tson[A] = Empty

  def pure[A](value: A): Tson[A] = ValOf(value)

  def option[A](value:Option[A]):Tson[A] = value.map(pure).getOrElse(empty)

  def array[A](value: Seq[Tson[A]]) = ArrayOf(value)

  def dict[A](value: Map[String, Tson[A]]) = DictOf(value)

  def seq[A](value: Seq[A]): Tson[A] = array(value.map(pure))

  def map[A](value: Map[String, A]): Tson[A] = dict(value.mapValues(pure))

  private[tson] case class ValOf[+A](value: A) extends Tson[A] {

    override def map[B](f: A => B): Tson[B] = ValOf(f(value))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = {
      f(value)
    }

    override def fold[B](ifEmpty: => B,
                         ifVal: A => B,
                         ifArray: Seq[Tson[A]] => B,
                         ifDict: Map[String, Tson[A]] => B): B = ifVal(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = true

    override def isArray: Boolean = false

    override def isDict: Boolean = false
  }

  private[tson] case class ArrayOf[+A](value: Seq[Tson[A]]) extends Tson[A] {
    override def map[B](f: A => B): Tson[B] = ArrayOf(value.map(_.map(f)))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = {
      ArrayOf(value.map { seq =>
        seq.flatMap(f)
      })
    }

    override def fold[B](ifEmpty: => B, ifVal: A => B, ifArray: Seq[Tson[A]] => B, ifDict: Map[String, Tson[A]] => B): B =
      ifArray(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = true

    override def isDict: Boolean = false
  }

  private[tson] case class DictOf[F[_], +A](value: Map[String, Tson[A]]) extends Tson[A] {
    private type StringMap[X] = Map[String, X]

    override def map[B](f: A => B): Tson[B] = DictOf(value.mapValues(_.map(f)))

    override def flatMap[B](f: A => Tson[B]): Tson[B] = DictOf(value.mapValues(_.flatMap(f)))

    override def fold[B](ifEmpty: => B, ifVal: A => B, ifArray: Seq[Tson[A]] => B, ifDict: Map[String, Tson[A]] => B): B =
      ifDict(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = true
  }

  private[tson] case object Empty extends Tson[Nothing] {
    override def map[B](f: Nothing => B): Tson[B] = this

    override def flatMap[B](f: Nothing => Tson[B]): Tson[B] = this

    override def fold[B](ifEmpty: => B,
                         ifVal: Nothing => B,
                         ifArray: Seq[Tson[Nothing]] => B,
                         ifDict: Map[String, Tson[Nothing]] => B): B = ifEmpty

    override def isEmpty: Boolean = true

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = false
  }


  implicit class TsonFOps[F[_], A](val tson: Tson[F[A]]) extends AnyVal {
    def sequence(implicit m: Monad[F]): F[Tson[A]] = Tson.sequence(tson)
  }

  private[tson] def sequence[F[_] : Monad, A](tson: Tson[F[A]]): F[Tson[A]] = {

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
