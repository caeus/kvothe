package edu.caeus.herbivicus.sower.data


case class Request[Body](
  route: List[String],
  query: Seq[(String, String)],
  body: Body
) {
  def currentSegment: Option[String] = route.headOption
  def next = route match{
    case _ :: tail => Some(Request(tail,query,body))
    case _ => None
  }
}

case class BranchReq[Segment, Query, Body](
  segment: Segment,
  query: Query,
  body: Body
)
