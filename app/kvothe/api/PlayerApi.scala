package kvothe.api

import scala.language.higherKinds

import kvothe.Ctx
import kvothe.domain.UserId

trait PlayerApi {
  def sheets: SheetsApi
}

object PlayerApi {
  def apply(ctx: Ctx, id: UserId): PlayerApi = new DefaultPlayerApi(ctx, id)
}

private[api] class DefaultPlayerApi(ctx: Ctx, id: UserId) extends PlayerApi {
  override def sheets: SheetsApi = new DefaultSheetsApi(ctx, id)

}
