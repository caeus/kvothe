package io.sower

trait CompileError {
  this: Throwable =>
  val msg: String = getMessage
  val request: Request[_]
}
case class NoBranchFoundError(request: Request[_])
  extends IllegalArgumentException(s"No branch defined for remainding route ${request.route} in request $request") with CompileError {
}

case class EndOfRouteFound(request: Request[_])
  extends IllegalArgumentException(s"Reached end of route(${request.original}) and request hasn't reached a leaf branch")
    with CompileError

case class ParsingError(
  override val msg: String,
  request: Request[_],
  cause:Option[Throwable]
) extends IllegalArgumentException(msg,cause.orNull) with CompileError