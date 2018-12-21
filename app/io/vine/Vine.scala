package io.vine

import scala.language.higherKinds

import cats._
import cats.implicits._
import io.vine.Vine.Combiner


sealed abstract class Vine[+A] {

  def map[B](f: A => B): Vine[B]

  def flatMap[B](f: A => Vine[B]): Vine[B]

  def combine[B >: A](
    ifEmpty: => B,
    ifArray: Seq[B] => B,
    ifDict: Map[String, B] => B
  ): B

  final def combineWith[B >:A](combiner: Combiner[B]): B = combine(
    combiner.empty,
    combiner.array,
    combiner.dict
  )

  def isEmpty: Boolean

  def isVal: Boolean

  def isArray: Boolean

  def isDict: Boolean
}


object Vine {

  trait Combiner[X] {
    def empty: X

    def array(seq: Seq[X]): X

    def dict(map: Map[String, X]): X

  }
  object Combiner{
    def apply[X](
      ifEmpty: => X,
      ifArray: Seq[X] => X,
      ifDict: Map[String, X] => X
    ) : Combiner[X] = new CombinerImpl[X](ifEmpty,ifArray,ifDict)
  }


  private class CombinerImpl[X](
    ifEmpty: => X,
    ifArray: Seq[X] => X,
    ifDict: Map[String, X] => X
  ) extends Combiner[X] {

    override def empty: X = ifEmpty
    override def array(seq: Seq[X]): X = ifArray(seq)
    override def dict(map: Map[String, X]): X = {
      ifDict(map)
    }

  }

  def empty[A]: Vine[A] = Empty

  def pure[A](value: A): Vine[A] = ValOf(value)

  def option[A](value: Option[A]): Vine[A] = value.map(pure).getOrElse(empty)

  def array[A](value: Seq[Vine[A]]) = ArrayOf(value)

  def dict[A](value: Map[String, Vine[A]]) = DictOf(value)

  def seq[A](value: Seq[A]): Vine[A] = array(value.map(pure))

  def map[A](value: Map[String, A]): Vine[A] = dict(value.mapValues(pure))

  private[vine] case class ValOf[+A](value: A) extends Vine[A] {

    override def map[B](f: A => B): Vine[B] = ValOf(f(value))

    override def flatMap[B](f: A => Vine[B]): Vine[B] = {
      f(value)
    }

    override def isEmpty: Boolean = false

    override def isVal: Boolean = true

    override def isArray: Boolean = false

    override def isDict: Boolean = false

    override def combine[B >: A](
      ifEmpty: => B,
      ifArray: Seq[B] => B,
      ifDict: Map[String, B] => B
    ): B = value
  }

  private[vine] case class ArrayOf[+A](value: Seq[Vine[A]]) extends Vine[A] {
    override def map[B](f: A => B): Vine[B] = ArrayOf(value.map(_.map(f)))

    override def flatMap[B](f: A => Vine[B]): Vine[B] = {
      ArrayOf(value.map { seq =>
        seq.flatMap(f)
      })
    }



    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = true

    override def isDict: Boolean = false

    override def combine[B >: A](
      ifEmpty: => B,
      ifArray: Seq[B] => B,
      ifDict: Map[String, B] => B
    ): B = ifArray(value.map(_.combine(ifEmpty,ifArray,ifDict)))
  }

  private[vine] case class DictOf[F[_], +A](value: Map[String, Vine[A]]) extends Vine[A] {
    private type StringMap[X] = Map[String, X]

    override def map[B](f: A => B): Vine[B] = DictOf(value.mapValues(_.map(f)))

    override def flatMap[B](f: A => Vine[B]): Vine[B] = DictOf(value.mapValues(_.flatMap(f)))

    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = true

    override def combine[B >: A](
      ifEmpty: => B,
      ifArray: Seq[B] => B,
      ifDict: Map[String, B] => B
    ): B = ifDict(value.mapValues(_.combine(ifEmpty,ifArray,ifDict)))
  }

  private[vine] case object Empty extends Vine[Nothing] {
    override def map[B](f: Nothing => B): Vine[B] = this

    override def flatMap[B](f: Nothing => Vine[B]): Vine[B] = this

    override def isEmpty: Boolean = true

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = false

    override def combine[B >: Nothing](
      ifEmpty: => B,
      ifArray: Seq[B] => B,
      ifDict: Map[String, B] => B
    ): B = ifEmpty
  }


  implicit class TsonFOps[F[_], A](val tson: Vine[F[A]]) extends AnyVal {
    def sequence(implicit m: Monad[F]): F[Vine[A]] = Vine.sequence(tson)
  }

  private[vine] def sequence[F[_] : Monad, A](tson: Vine[F[A]]): F[Vine[A]] = {

    def recFold(tson: Vine[F[A]]): F[Vine[A]] = {
      tson match {
        case ValOf(fa) => fa.map(ValOf(_))
        case ArrayOf(seq) =>
          seq.map(recFold).toList.sequence.map(ArrayOf(_))
        case DictOf(map) =>
          map.mapValues(recFold).map {
            case (path, value) => value.map(path -> _)
          }.toList.sequence.map(_.toMap).map(DictOf(_))
        case empty @ Empty =>
          Monad[F].pure(empty)
      }
    }

    recFold(tson)
  }


}
