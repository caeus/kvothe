package io.sower

import scala.language.{higherKinds, reflectiveCalls}
import scala.util.{Failure, Success, Try}

import cats.arrow.FunctionK

trait SowerDSL[F[_], Input, Output] {
  sower: Sower[F, Input, Output] =>
  object syntax{
    def fork[From](branches: Branches[From] => Branches[From]): sower.Sprout[From] = {
      val branchBuilder = new Branches[From](Nil)
      sower.ramification(branches(branchBuilder).value.reverse)
    }

    def leaf[From](implicit pipe: Pipe[From, Output]): sower.Sprout[From] = sower.leaf(pipe)

    private[syntax] class BranchBuilder[From, Segment, Payload, To] private[sower](
      prevBranches:List[Branch[From]],
      branchSelector: BranchSelector[Segment],
      inputParser: Pipe[Input, Try[Payload]],
      resolver: BranchReq[Segment, Payload] => From => F[To]
    ) {
      def into(sprout: sower.Sprout[To]): Branches[From] = {
        new Branches(sower.branch(branchSelector, inputParser, resolver, sprout)::prevBranches )
      }
    }

    private[syntax] case class BranchResolverBuilder[From, Segment, Payload] private[sower](
      prevBranches:List[Branch[From]],
      branchSelector: BranchSelector[Segment],
      inputParser: Pipe[Input, Try[Payload]]
    ) {

      def payload[NewPayload](inputParser: Pipe[Input, Try[NewPayload]]): BranchResolverBuilder[From, Segment, NewPayload]
      = copy(inputParser = inputParser)

      def segment[NewSegment](segmentParser: Pipe[Option[String], Try[NewSegment]]): BranchResolverBuilder[From, NewSegment, Payload]
      = copy(branchSelector = branchSelector.copy(func = segmentParser.apply))

      def apply[To](func: BranchReq[Segment, Payload] => From => F[To]): BranchBuilder[From, Segment, Payload, To]
      = new BranchBuilder[From, Segment, Payload, To](prevBranches,branchSelector, inputParser, func)
    }
    private[sower] class Branches[From] private[sower](val value:List[Branch[From]]) {
      def branch(name: String): BranchResolverBuilder[From, Unit, Input] = {
        BranchResolverBuilder(value,BranchSelector(name, _.map(_ => Failure(new Exception("This branch doesn't take params")))
          .getOrElse(Success(()))): BranchSelector[Unit],
          sower.normalInputParser
        )
      }
    }


    class Ofable[X[_]](func: FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[X[Y]]})#L]) {
      def of[Val](sprout: sower.Sprout[Val]): sower.Sprout[X[Val]] = func(sprout)
    }

    def option = new Ofable[Option](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Option[Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Option[A]] = fa.optional
    })

    def seq = new Ofable[Seq](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Seq[Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Seq[A]] = fa.array
    })

    def map = new Ofable[({type L[Y] = Map[String, Y]})#L](new FunctionK[sower.Sprout, ({type L[Y] = sower.Sprout[Map[String, Y]]})#L] {
      override def apply[A](fa: sower.Sprout[A]): sower.Sprout[Map[String, A]] = fa.dictionary
    })


    val asString = Pipe[Option[String], Try[String]] {
      case Some(x) => Success(x)
      case None => Failure(new Exception("This is not what I want"))
    }

    def as[NewBody](implicit inputParser: Pipe[Input, Try[NewBody]]): Pipe[Input, Try[NewBody]] = inputParser
  }

}
