package edu.caeus.herbivicus.vine

import scala.language.higherKinds

import cats._
import cats.implicits._

case class VineT[F[_] : Monad, A](value: F[Vine[A]]) {
  private val F = Monad[F]

  def map[B](f: A => B): VineT[F, B] = VineT(value.map(_.map(f)))

  def semiflatMap[B](f: A => F[B]): VineT[F, B] =
    flatMap(a => VineT.liftF(f(a)))

  def flatMap[B](f: A => VineT[F, B]): VineT[F, B] = flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Vine[B]]): VineT[F, B] = {
    VineT(value.flatMap(vineA => vineA.map(f).sequence)
      .map(_.flatMap(identity)))
  }

  def flatTransform[B](f: Vine[A] => F[Vine[B]]): VineT[F, B] =
    VineT(F.flatMap(value)(f))

  def transform[B](f: Vine[A] => Vine[B]): VineT[F, B] =
    VineT(F.map(value)(f))

  def subflatMap[B](f: A => Vine[B]): VineT[F, B] =
    transform(_.flatMap(f))

  def growVal[B](branch: String)(f: A => F[B]): VineT[F, B] = {
    grow(branch)(f)(Vine.pure)
  }

  def growOpt[B](branch: String)(f: A => F[Option[B]]): VineT[F, B] = {
    grow(branch)(f)(Vine.option)
  }

  def growSeq[B](branch: String)(f: A => F[Seq[B]]): VineT[F, B] = {
    grow(branch)(f)(Vine.seq)
  }

  def growMap[B](branch: String)(f: A => F[Map[String, B]]): VineT[F, B] = {
    grow(branch)(f)(Vine.map)
  }

  def grow[B, C](branch: String)(f: A => F[B])(into: B => Vine[C]): VineT[F, C] = {
    growBranch(branch).semiflatMap(f).subflatMap(into)
  }

  def collapse: F[Vine[A]] = value

  def growBranch(branch: String): VineT[F, A] = subflatMap(a => Vine.map(Map(branch -> a)))

}

object VineT {

  private[vine] class PurePartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: A)
      (implicit F: Monad[F]): VineT[F, A] = VineT(Monad[F].pure(Vine.pure(value)))
  }
  def pure[F[_]] = new PurePartiallyApplied[F]()

  private[vine] class FromTsonPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: Vine[A])
      (implicit F: Monad[F]): VineT[F, A] = VineT(Monad[F].pure(value))
  }
  def fromTson[F[_]] = new FromTsonPartiallyApplied[F]()

  def liftF[F[_], A](fa: F[A])
    (implicit F: Monad[F]): VineT[F, A] = VineT(F.map(fa)(Vine.pure))

}
