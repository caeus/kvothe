package kvothe.rost

trait Action[In, Fragment, Query, Body] {
  def value: In
  def req: Req[Fragment, Query, Body]
}

object Action {
  private[rost] case class ActionImpl[In, Fragment, Query, Body](
    value: In,
    req: Req[Fragment, Query, Body]
  ) extends Action[In, Fragment, Query, Body]

  def apply[In, Fragment, Query, Body](
    value: In,
    req: Req[Fragment, Query, Body]
  ): Action[In, Fragment, Query, Body] = ActionImpl(value, req)
}


trait Req[Segment, Query, Body] {
  def next: NormalReq
  def segment: Segment
  def rawSegment: String
  def query: Query
  def body: Body
  def build[In](in: In): Action[In, Segment, Query, Body]
}
object Req {
  def apply[In, Segment, Query, Body](
    segment: Segment,
    rawSegment: String,
    query: Query,
    body: Body
  ): Req[Segment, Query, Body] = {
    new DefaultReq(segment: Segment, rawSegment, query: Query, body: Body)
  }
}

class DefaultReq[Segment, Query, Body](
  val segment: Segment,
  val rawSegment: String,
  val query: Query,
  val body: Body
)
  extends Req[Segment, Query, Body] {

  override def build[In](in: In): Action[In, Segment, Query, Body] = ???

  override def next: NormalReq = ???
}