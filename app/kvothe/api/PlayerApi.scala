package kvothe.api

import scala.language.higherKinds
import scala.util.Try

import kvothe.Ctx
import kvothe.domain.UserId
import monix.eval.Task

trait PlayerApi extends ForumApi{
  def sheets: SheetsApi
}

object PlayerApi {
  def apply(ctx: Ctx, id: UserId): PlayerApi = new DefaultPlayerApi(ctx, id)
}

private[api] class DefaultPlayerApi(ctx: Ctx, id: UserId) extends PlayerApi {
  override def sheets: SheetsApi = new DefaultSheetsApi(ctx, id)

  override def threads(
    query: String,
    offset: Long,
    limit: Long
  ): Task[Seq[ThreadRef]] = ???

  override def thread(id: String): Task[Option[ThreadApi]] = ???

  override def start(startThreadRequest: StartThreadRequest): Task[Try[StartThreadResponse]] = ???
}

