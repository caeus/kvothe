package kvothe.utility.tson

import scala.language.higherKinds

import Tson.{ArrayOf, DictOf, Empty, ValOf}
import cats._
import cats.implicits._

abstract class TCursor[F[_], A] {
  def down[B](path: String)(f: A => F[B]): TCursor[F, B]

  def downOpt[B](path: String)(f: A => F[Option[B]]): TCursor[F, B]

  def downArr[B](path: String)(f: A => F[Seq[B]]): TCursor[F, B]

  def downMap[B](path: String)(f: A => F[Map[String, B]]): TCursor[F, B]

  def collapse: F[Tson[A]]

}

class DefaultTCursor[F[_] : Monad, A](value: F[Tson[A]]) extends TCursor[F, A] {
  def prop[Val](name: String, value: Tson[Val]): Tson[Val] = DictOf(Map(name -> value))

  private def proceedWith[B](f: A => F[Tson[B]]): TCursor[F, B] =
    TCursor(value.flatMap(
      _.map(f).sequence.map(_.flatMap(identity))
    ))

  override def down[B](path: String)(f: A => F[B]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map(b => prop(path, ValOf(b)))))
  }

  override def downOpt[B](path: String)(f: A => F[Option[B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map {
      maybeB => prop(path, maybeB.map(ValOf(_)).getOrElse(Empty))
    }))
  }

  override def downArr[B](path: String)(f: A => F[Seq[B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map {
      seqOfB =>
        prop(path, ArrayOf(seqOfB.map(ValOf(_))))
    }))
  }

  override def downMap[B](path: String)(f: A => F[Map[String, B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map {
      mapOfB =>
        prop(path, DictOf(mapOfB.mapValues(ValOf(_))))
    }))
  }

  override def collapse: F[Tson[A]] = value
}


object TCursor {

  private[tson] class PurePartiallyApplied[F[_]](val dummy:Boolean=true) extends AnyVal{
    def apply[A](value:A)(implicit F:Monad[F]):TCursor[F,A] = new DefaultTCursor(Monad[F].pure(Tson.pure(value)))
  }
  def pure[F[_]] = new PurePartiallyApplied[F]()

  private[tson] class FromTsonPartiallyApplied[F[_]](val dummy:Boolean=true) extends AnyVal{
    def apply[A](value:Tson[A])(implicit F:Monad[F]):TCursor[F,A] = new DefaultTCursor(Monad[F].pure(value))
  }

  def fromTson[F[_]] = new FromTsonPartiallyApplied[F]()

  def apply[F[_]:Monad,A](ftsona:F[Tson[A]]): TCursor[F, A] = new DefaultTCursor(ftsona)

  def liftF[F[_],A](fa:F[A])(implicit F:Monad[F]):TCursor[F,A]= new DefaultTCursor(F.map(fa)(Tson.pure))

  private[TCursor] class Builder[F[_]](implicit monad: Monad[F]) {
    def pure[A](value: A): TCursor[F, A] = new DefaultTCursor(Monad[F].pure(ValOf(value)))

    def lift[A](value: Tson[A]): TCursor[F, A] = new DefaultTCursor(Monad[F].pure(value))

    def from[A](value: F[Tson[A]]): TCursor[F, A] = new DefaultTCursor(value)
  }

}
