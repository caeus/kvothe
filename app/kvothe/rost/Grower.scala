package kvothe.rost

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import cats.Monad
import kvothe.utility.vine.{Vine, VineT}

abstract class Grower[F[_] : Monad, In, Format] {
  //Fixme body type should be something that allows laziness of reading
  final def handle(value: In, req: NormalReq): Try[F[Format]] = {
    compile(req).map {
      _.run(VineT.pure[F](value))
    }
  }

  def compile(req: NormalReq): Try[Thunk[F, In, Format]]

  //Fixme
  def description: Unit
}

object Grower {
  def fork[F[_] : Monad, In, Format](branches: (BranchSelectorBuilder[F, In, Format] => Branch[F, In, Format])*):
  Grower[F, In, Format] = {
    val builder = new BranchSelectorBuilder[F, In, Format]()

    new ForkGrower(branches.map(func => func(builder)))
  }

  private[rost] class ForkGrower[F[_] : Monad, In, Format](
    branches: Seq[Branch[F, In, Format]]
  ) extends Grower[F, In, Format] {
    override def description: Unit = ???

    override def compile(req: NormalReq): Try[Thunk[F, In, Format]] = {
      req.currSegment.map {
        segment =>
          branches.iterator.map(_.compile(segment, req)).dropWhile(_.isEmpty).map(_.get).toIterable.headOption
            .fold(Failure(new Exception(s"No branch matched path $segment")): Try[Thunk[F, In, Format]])(identity)
      }.getOrElse(Failure(new Exception(s"Malformed path ${req.path} could not be matched")))

    }
  }
  private[rost] class LeafGrower[F[_] : Monad, In, Format](implicit format: Pipe[Vine[In], Format]) extends Grower[F, In, Format] {
    override def compile(req: NormalReq): Try[Thunk[F, In, Format]] = {
      if (req.isLast) {
        Success(Thunk.leaf[F, In, Format](format)): Try[Thunk[F, In, Format]]
      } else Failure(new Exception(s"Malformed path ${req.path} could not be matched"))
    }

    override def description: Unit = ???
  }

  def leaf[F[_] : Monad, In, Format](implicit format: Pipe[Vine[In], Format]): Grower[F, In, Format] = new LeafGrower

}

