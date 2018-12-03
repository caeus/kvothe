package kvothe.utility.tson

import cats.FlatMap
import cats._
import cats.implicits._
import Tson.{ArrayOf, DictOf, Empty, ValOf}

import scala.language.higherKinds

abstract class TCursor[F[_], A] {
  def down[B](path: String)(f: A => F[B]): TCursor[F, B]

  def downOpt[B](path: String)(f: A => F[Option[B]]): TCursor[F, B]

  def downArr[B](path: String)(f: A => F[Seq[B]]): TCursor[F, B]

  def downMap[B](path: String)(f: A => F[Map[String, B]]): TCursor[F, B]

  def fold: F[Tson[A]]

}

class DefaultTCursor[F[_] : Monad, A](value: F[Tson[A]]) extends TCursor[F, A] {
  def prop[Val](name: String,value: Tson[Val]): Tson[Val] = DictOf(Map(name -> value))

  private def proceedWith[B](f: A => F[Tson[B]]): TCursor[F, B] =
    TCursor(value.flatMap(
      _.map(f).foldRec.map(_.flatMap(identity))
    ))

  override def down[B](path: String)(f: A => F[B]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map(b=>prop(path,ValOf(b)))))
  }

  override def downOpt[B](path: String)(f: A => F[Option[B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map{
      maybeB=> prop(path,maybeB.map(ValOf(_)).getOrElse(Empty))
    }))
  }

  override def downArr[B](path: String)(f: A => F[Seq[B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map{
      seqOfB=>
        prop(path,ArrayOf(seqOfB.map(ValOf(_))))
    }))
  }

  override def downMap[B](path: String)(f: A => F[Map[String, B]]): TCursor[F, B] = {
    proceedWith(f.andThen(_.map{
      mapOfB=>
        prop(path,DictOf(mapOfB.mapValues(ValOf(_))))
    }))
  }

  override def fold: F[Tson[A]] = value
}


object TCursor {

  final class PurePartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](value:A)(implicit F: Monad[F]): TCursor[F,A] = new DefaultTCursor(F.pure(ValOf(value)))
  }
  def pure[F[_]]=new PurePartiallyApplied[F]()

  final class FromTsonPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](tson:Tson[A])(implicit F: Monad[F]): TCursor[F,A] = new DefaultTCursor(F.pure(tson))
  }
  def fromTson[F[_]]=new FromTsonPartiallyApplied[F]()

  def liftF[F[_],A](fa:F[A])(implicit F:Monad[F]): TCursor[F,A] = new DefaultTCursor(F.map(fa)(ValOf(_)))

  def apply[F[_]:Monad,A](ftsona:F[Tson[A]]): TCursor[F, A] =new DefaultTCursor(ftsona)

}
