package kvothe.utility.vine

import scala.language.higherKinds

import kvothe.api.{PlayerApi, SheetApi, SheetsApi}
import kvothe.domain.SheetId
import kvothe.rost.{Branch, Command, Grower, NormalBody, NormalQuery}
import monix.eval.Task
import play.api.libs.json.JsValue

package object dsl {

  trait GrowerBuilder[F[_], Format] {
    def fork[In](branches: BranchSelectorBuilder[F, In, Format] => Branch[F, In, Format]): Grower[F, In, Format]
    def leaf[In]: Grower[F, In, Format]
  }

  trait BranchBuilder[F[_], In, Segment, Query, Body, Out, Format] {
    def towards(grower: Grower[F, Out, Format]): Branch[F, In, Format]
  }

  trait BranchResolverBuilder[F[_], In, Segment, Query, Body, Format] {
    def query[NewQuery](string: String): BranchResolverBuilder[F, In, Segment, NewQuery, Body, Format]
    def body[NewBody](string: String): BranchResolverBuilder[F, In, Segment, Query, NewBody, Format]
    def resolve[Out](func: Command[In, Segment, Query, Body] => F[Out]): BranchBuilder[F, In, Segment, Query, Body, Out, Format]
  }
  trait BranchSelectorBuilder[F[_], In, Format] {
    def branch[Segment](string: String): BranchResolverBuilder[F, In, Segment, NormalQuery, NormalBody, Format]
  }

  val rost: GrowerBuilder[Task, JsValue] = ???

  rost.fork[PlayerApi](
    _.branch[String]("sheets")
      .query[Map[String, String]]("qwe").body[JsValue]("asddqe")
      .resolve(api => Task eval api.value.sheets)
      towards rost.fork[SheetsApi](
      _.branch[SheetId]("one")
        .resolve(cmd => cmd.value.one(cmd.segment.value))
        towards rost.leaf[SheetApi].?
    ))
}
