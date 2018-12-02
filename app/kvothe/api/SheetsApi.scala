package kvothe.api

import kvothe.Ctx
import kvothe.domain._
import monix.eval.Task

import scala.language.higherKinds

trait SheetsApi {
  def entries: Task[Seq[SheetEntry]]

  def one(id: String): Task[Option[SheetApi]]

  def create(request: SheetCreationRequest): Task[SheetCreationResponse]
}

class DefaultSheetsApi(ctx: Ctx, playerId: UserId) extends SheetsApi {
  override def entries: Task[Seq[SheetEntry]] = ctx.grantsIO.allOf(playerId).map {
    grants =>
      grants.map {
        grant =>
          SheetEntry(grant.sheetId.value)
      }
  }

  override def one(id: String): Task[Option[SheetApi]] = ctx.grantsIO.bySheetId(
    playerId,
    SheetId(id)
  ).map(_.map(grant => new DefaultSheetApi(ctx, playerId, grant)))

  override def create(request: SheetCreationRequest): Task[SheetCreationResponse] = {
    ???
  }
}
