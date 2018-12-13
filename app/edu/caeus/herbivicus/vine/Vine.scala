package edu.caeus.herbivicus.vine

import scala.language.higherKinds

import cats._
import cats.implicits._
import edu.caeus.herbivicus.vine.Vine.Folder


sealed abstract class Vine[+A] {

  def map[B](f: A => B): Vine[B]

  def flatMap[B](f: A => Vine[B]): Vine[B]

  def fold[B](
    ifEmpty: => B,
    ifVal: A => B,
    ifArray: Seq[Vine[A]] => B,
    ifDict: Map[String, Vine[A]] => B
  ): B

  final def foldWith[B](folder: Folder[A, B]): B = fold(
    folder.empty,
    folder.value,
    array => folder.array(array.map(_.foldWith(folder))),
    dict => folder.dict(dict.mapValues(_.foldWith(folder)))
  )

  def isEmpty: Boolean

  def isVal: Boolean

  def isArray: Boolean

  def isDict: Boolean
}


object Vine {

  trait Folder[-In, Out] {
    def empty: Out

    def value(in: In): Out

    def array(seq: Seq[Out]): Out

    def dict(map: Map[String, Out]): Out

  }
  trait PartialFolder[Out] {
    def empty: Out

    def array(seq: Seq[Out]): Out

    def dict(map: Map[String, Out]): Out

    final def apply[In](f: In => Out): Folder[In, Out] = new FolderImpl(empty, f, array, dict)
  }
  object PartialFolder {
    def apply[Out](
      ifEmpty: => Out,
      ifArray: Seq[Out] => Out,
      ifDict: Map[String, Out] => Out
    ) =
      new PartialFolderImpl[Out](ifEmpty, ifArray, ifDict)
  }
  private[Vine] class PartialFolderImpl[Out](
    ifEmpty: => Out,
    ifArray: Seq[Out] => Out,
    ifDict: Map[String, Out] => Out
  ) extends PartialFolder[Out] {
    override def empty: Out = ifEmpty

    override def array(seq: Seq[Out]): Out = ifArray(seq)

    override def dict(map: Map[String, Out]): Out = ifDict(map)
  }

  private class FolderImpl[In, Out](
    ifEmpty: => Out,
    ifValue: In => Out,
    ifArray: Seq[Out] => Out,
    ifDict: Map[String, Out] => Out
  ) extends Folder[In, Out] {
    override def empty: Out = ifEmpty

    override def value(in: In): Out = ifValue(in)

    override def array(seq: Seq[Out]): Out = ifArray(seq)

    override def dict(map: Map[String, Out]): Out = ifDict(map)

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

    override def fold[B](
      ifEmpty: => B,
      ifVal: A => B,
      ifArray: Seq[Vine[A]] => B,
      ifDict: Map[String, Vine[A]] => B
    ): B = ifVal(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = true

    override def isArray: Boolean = false

    override def isDict: Boolean = false
  }

  private[vine] case class ArrayOf[+A](value: Seq[Vine[A]]) extends Vine[A] {
    override def map[B](f: A => B): Vine[B] = ArrayOf(value.map(_.map(f)))

    override def flatMap[B](f: A => Vine[B]): Vine[B] = {
      ArrayOf(value.map { seq =>
        seq.flatMap(f)
      })
    }

    override def fold[B](
      ifEmpty: => B,
      ifVal: A => B,
      ifArray: Seq[Vine[A]] => B,
      ifDict: Map[String, Vine[A]] => B
    ): B =
      ifArray(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = true

    override def isDict: Boolean = false
  }

  private[vine] case class DictOf[F[_], +A](value: Map[String, Vine[A]]) extends Vine[A] {
    private type StringMap[X] = Map[String, X]

    override def map[B](f: A => B): Vine[B] = DictOf(value.mapValues(_.map(f)))

    override def flatMap[B](f: A => Vine[B]): Vine[B] = DictOf(value.mapValues(_.flatMap(f)))

    override def fold[B](
      ifEmpty: => B,
      ifVal: A => B,
      ifArray: Seq[Vine[A]] => B,
      ifDict: Map[String, Vine[A]] => B
    ): B =
      ifDict(value)

    override def isEmpty: Boolean = false

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = true
  }

  private[vine] case object Empty extends Vine[Nothing] {
    override def map[B](f: Nothing => B): Vine[B] = this

    override def flatMap[B](f: Nothing => Vine[B]): Vine[B] = this

    override def fold[B](
      ifEmpty: => B,
      ifVal: Nothing => B,
      ifArray: Seq[Vine[Nothing]] => B,
      ifDict: Map[String, Vine[Nothing]] => B
    ): B = ifEmpty

    override def isEmpty: Boolean = true

    override def isVal: Boolean = false

    override def isArray: Boolean = false

    override def isDict: Boolean = false
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
