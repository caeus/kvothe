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


trait Req[Fragment, Query, Body] {
  def next: NormalReq
  def fragment: Fragment
  def query: Query
  def body: Body
  def build[In](in: In): Action[In, Fragment, Query, Body]
}
object Req {
  def apply[In, Fragment, Query, Body](
    fragment: Fragment,
    query: Query,
    body: Body
  ): Req[Fragment, Query, Body] = {
    new DefaultReq(fragment: Fragment, query: Query, body: Body)
  }
}

class DefaultReq[Fragment, Query, Body](
  val fragment: Fragment,
  val query: Query,
  val body: Body
)
  extends Req[Fragment, Query, Body] {

  override def build[In](in: In): Action[In, Fragment, Query, Body] = ???

  override def next: NormalReq = ???
}