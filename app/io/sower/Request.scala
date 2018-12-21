package io.sower

import scala.util.Try
import scala.util.matching.Regex


case class Request[Payload](
  route: List[String],
  payload: Payload,
  original: List[String],
) {
  def currentSegment: Option[String] = route.headOption

  def next: Option[Request[Payload]] = route match {
    case _ :: tail => Some(Request(tail, payload, original))
    case _ => None
  }
}

object Request{
  def apply[Payload](
    route: List[String],
    payload: Payload,
  ):Request[Payload] = Request(route,payload,route)
}

case class BranchReq[Segment, Payload](
  segment: Segment,
  payload: Payload
)
case class BranchSelector[X](name: String, func: Option[String] => Try[X]) {
  name.ensuring(_.indexOf(':') == -1, s"""Semicolon must not be used in names of branch: "$name"""")
  private lazy val matcher = s"${Regex.quote(name)}(:.*)?".r

  def matchRawSegment(segment: String): Option[Try[X]] = {
    segment match {
      case matcher(content) => Some(func(Option(content).map(_.substring(1))))
      case _ => None
    }
  }
}