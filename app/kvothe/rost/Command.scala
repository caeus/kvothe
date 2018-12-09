package kvothe.rost

case class Command[In, Segment, Query, Body](value: In, segment: Segment, req: Request[Query, Body])

case class Request[Query, Body](
                                 path: List[String],
                                 query: Query,
                                 body: Body
                               ) {

  def currSegment: Option[String] =path.headOption
  def next: Option[Request[Query, Body]] = path match {
    case _ :: tail => Some(Request(tail,query,body))
    case _ => None
  }
}
