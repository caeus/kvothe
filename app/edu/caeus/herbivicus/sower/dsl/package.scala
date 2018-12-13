package edu.caeus.herbivicus.sower

import scala.language.{higherKinds, reflectiveCalls}
import scala.util.Try

import cats.arrow.FunctionK
import edu.caeus.herbivicus.sower.data.{BranchReq, Request}

package object dsl {

  final class SowerDSL[F[_], Format](val sower: Sower[F, Format]) {
    type Sync[X] = Try[X]
    type QueryParams = Seq[(String, String)]
    type RequestBody = Format
    type BranchSelector[Segment] = Pipe[Option[String], Option[Segment]]
    type QueryParser[Query] = Pipe[QueryParams, Sync[Query]]
    type BodyParser[Body] = Pipe[RequestBody, Sync[Body]]
    type NormalReq = Request[RequestBody]
    type Writer[X] = Pipe[X, Format]

    def fork[In](branches: (BranchSelectorBuilder[In] => sower.Branch[In])*): sower.Sprout[In] = {
      val branchBuilder = new BranchSelectorBuilder[In]
      sower.ramification(branches.map(_ (branchBuilder)))
    }

    def leaf[In](implicit pipe: Pipe[In, Format]): sower.Sprout[In] = sower.leaf(pipe)

    private[dsl] class BranchBuilder[In, Segment, Query, Body, Out] private[sower](
      branchSelector: BranchSelector[Segment],
      queryParser: QueryParser[Query],
      bodyParser: BodyParser[Body],
      resolver: BranchReq[Segment, Query, Body] => In => F[Out]
    ) {
      def into(sprout: sower.Sprout[Out]): sower.Branch[In] = {
        sower.branch(branchSelector, queryParser, bodyParser, resolver, sprout)
      }
    }

    private[dsl] case class BranchResolverBuilder[In, Segment, Query, Body] private[sower](
      branchSelector: BranchSelector[Segment],
      queryParser: QueryParser[Query],
      bodyParser: BodyParser[Body]
    ) {
      def query[NewQuery](queryParser: QueryParser[NewQuery]): BranchResolverBuilder[In, Segment, NewQuery, Body]
      = copy(queryParser = queryParser)

      def body[NewBody](bodyParser: BodyParser[NewBody]): BranchResolverBuilder[In, Segment, Query, NewBody]
      = copy(bodyParser = bodyParser)

      def apply[Out](func: BranchReq[Segment, Query, Body] => In => F[Out]): BranchBuilder[In, Segment, Query, Body, Out]
      = new BranchBuilder[In, Segment, Query, Body, Out](branchSelector, queryParser, bodyParser, func)
    }
    private[sower] class BranchSelectorBuilder[In] private[sower]() {
      def branch[Segment](branchSelector: BranchSelector[Segment]): BranchResolverBuilder[In, Segment, QueryParams, RequestBody] = {
        BranchResolverBuilder(branchSelector: BranchSelector[Segment],
          sower.normalQueryParser,
          sower.normalBodyParser
        )
      }
    }

    implicit def stringAsBranchSelector(value: String): BranchSelector[String] = Pipe[Option[String], Option[String]] {
      _.filter(_ == value)
    }

    def Segment: BranchSelector[String] = Pipe.id[Option[String]]

    def ---[In, Segment, Query, Body]: BranchReq[Segment, Query, Body] => In => F[In] =
      _ => sower.monadOfF.pure


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


    def as[NewBody](implicit bodyParser: Pipe[Format, Try[NewBody]]): BodyParser[NewBody] = bodyParser
  }
  def from[F[_], Format](sower: Sower[F, Format]) = new SowerDSL[F, Format](sower)
}
