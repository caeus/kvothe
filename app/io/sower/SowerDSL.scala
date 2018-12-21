package io.sower

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import cats.arrow.FunctionK

trait SowerDSL[F[_], Input, Output] {
  sower: Sower[F, Input, Output] =>
  object syntax{
    def fork[From](branches: (BranchSelectorBuilder[From] => sower.Branch[From])*): sower.Sprout[From] = {
      val branchBuilder = new BranchSelectorBuilder[From]
      sower.ramification(branches.map(_ (branchBuilder)))
    }

    def leaf[From](implicit pipe: Pipe[From, Output]): sower.Sprout[From] = sower.leaf(pipe)

    private[syntax] class BranchBuilder[From, Segment, Payload, To] private[sower](
      branchSelector: BranchSelector[Segment],
      inputParser: Pipe[Input, Try[Payload]],
      resolver: BranchReq[Segment, Payload] => From => F[To]
    ) {
      def into(sprout: sower.Sprout[To]): sower.Branch[From] = {
        sower.branch(branchSelector, inputParser, resolver, sprout)
      }
    }

    private[syntax] case class BranchResolverBuilder[From, Segment, Payload] private[sower](
      branchSelector: BranchSelector[Segment],
      inputParser: Pipe[Input, Try[Payload]]
    ) {

      def payload[NewPayload](inputParser: Pipe[Input, Try[NewPayload]]): BranchResolverBuilder[From, Segment, NewPayload]
      = copy(inputParser = inputParser)

      def segment[NewSegment](segmentParser: Pipe[Option[String], Try[NewSegment]]): BranchResolverBuilder[From, NewSegment, Payload]
      = copy(branchSelector = branchSelector.copy(func = segmentParser.apply))

      def apply[To](func: BranchReq[Segment, Payload] => From => F[To]): BranchBuilder[From, Segment, Payload, To]
      = new BranchBuilder[From, Segment, Payload, To](branchSelector, inputParser, func)
    }
    private[sower] class BranchSelectorBuilder[From] private[sower]() {
      def branch[Segment](branchSelector: BranchSelector[Segment]): BranchResolverBuilder[From, Segment, Input] = {
        BranchResolverBuilder(branchSelector: BranchSelector[Segment],
          sower.normalInputParser
        )
      }
    }
    implicit def stringAsBranchSelector(value: String): BranchSelector[Unit] = BranchSelector(value,
      _.map(_ => Failure(new Exception("This branch doesn't take params"))).getOrElse(Success(())))

    class Ofable[X[_]](func: FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[X[Y]]})#L]) {
      def of[Val](sprout: sower.Sprout[Val]): sower.Sprout[X[Val]] = func(sprout)
    }

    def option = new Ofable[Option](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Option[Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Option[A]] = fa.?
    })

    def seq = new Ofable[Seq](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Seq[Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Seq[A]] = fa.<>
    })

    def map = new Ofable[({type L[Y] = Map[String, Y]})#L](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Map[String, Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Map[String, A]] = fa.<:>
    })


    val asString = Pipe[Option[String], Try[String]] {
      case Some(x) => Success(x)
      case None => Failure(new Exception("This is not what I want"))
    }

    def as[NewBody](implicit inputParser: Pipe[Input, Try[NewBody]]): Pipe[Input, Try[NewBody]] = inputParser
  }

}
