package kvothe.utility.tson

import scala.language.higherKinds

import Tson.{ArrayOf, DictOf, Empty, ValOf}
import cats._
import cats.implicits._

abstract class TPot[F[_], A] {

  def grow[B,C](branch: String)(f:A => F[B])(like: B=>Tson[C]):TPot[F,C]

  def growVal[B](branch: String)(f: A => F[B]): TPot[F, B]

  def growOpt[B](branch: String)(f: A => F[Option[B]]): TPot[F, B]

  def growSeq[B](branch: String)(f: A => F[Seq[B]]): TPot[F, B]

  def growMap[B](branch: String)(f: A => F[Map[String, B]]): TPot[F, B]

  def collapse: F[Tson[A]]

}

class DefaultTPot[F[_] : Monad, A](value: F[Tson[A]]) extends TPot[F, A] {

  override def growVal[B](branch: String)(f: A => F[B]): TPot[F, B] = {
    grow(branch)(f)(Tson.pure)
  }

  override def growOpt[B](branch: String)(f: A => F[Option[B]]): TPot[F, B] = {
    grow(branch)(f)(Tson.option)
  }

  override def growSeq[B](branch: String)(f: A => F[Seq[B]]): TPot[F, B] = {
    grow(branch)(f)(Tson.seq)
  }

  override def growMap[B](branch: String)(f: A => F[Map[String, B]]): TPot[F, B] = {
    grow(branch)(f)(Tson.map)
  }

  override def collapse: F[Tson[A]] = value

  override def grow[B, C](branch: String)(f: A => F[B])(like: B => Tson[C]): TPot[F, C] = {
    TPot(value.flatMap{
      _.map(f).sequence
    }.map(_.flatMap{
      b=> Tson.dict(Map(branch->like(b)))
    }))
  }
}


object TPot {

  private[tson] class PurePartiallyApplied[F[_]](val dummy:Boolean=true) extends AnyVal{
    def apply[A](value:A)(implicit F:Monad[F]):TPot[F,A] = new DefaultTPot(Monad[F].pure(Tson.pure(value)))
  }
  def pure[F[_]] = new PurePartiallyApplied[F]()

  private[tson] class FromTsonPartiallyApplied[F[_]](val dummy:Boolean=true) extends AnyVal{
    def apply[A](value:Tson[A])(implicit F:Monad[F]):TPot[F,A] = new DefaultTPot(Monad[F].pure(value))
  }
  def fromTson[F[_]]=new FromTsonPartiallyApplied[F]()

  def liftF[F[_],A](fa:F[A])(implicit F:Monad[F]): TPot[F,A] = new DefaultTPot(F.map(fa)(ValOf(_)))

  def apply[F[_]:Monad,A](ftsona:F[Tson[A]]): TPot[F, A] =new DefaultTPot(ftsona)

}
