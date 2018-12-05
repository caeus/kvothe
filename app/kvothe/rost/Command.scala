package kvothe.rost

case class Command[In, Segment, Query, Body](value: In, segment: MatchedSegment[Segment], req: Request[Query, Body])

case class Request[Query, Body](
                                 path: Vector[String],
                                 cursor: Int,
                                 query: Query,
                                 body: Body
                               ) {
  def isLast: Boolean = cursor >= path.size
  def currSegment=path.lift(cursor)
  def next: Option[Request[Query, Body]] = if (isLast) None
  else Some(Request(path, cursor + 1, query, body))
}
