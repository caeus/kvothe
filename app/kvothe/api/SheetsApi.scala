package kvothe.api

import scala.language.higherKinds

import kvothe.Ctx
import kvothe.domain._
import monix.eval.Task
import play.api.libs.json.JsValue

trait SheetsApi {
  def entries: Task[Seq[SheetEntry]]

  def one(id: SheetId): Task[Option[SheetApi]]

  def create(request: CreateSheetRequest): Task[SheetCreationResponse]
}

class DefaultSheetsApi(ctx: Ctx, playerId: UserId) extends SheetsApi {
  override def entries: Task[Seq[SheetEntry]] = ctx.grantsIO.allOf(playerId).map {
    grants =>
      grants.map {
        grant =>
          SheetEntry(grant.sheetId.value)
      }
  }

  override def one(id: SheetId): Task[Option[SheetApi]] = ctx.grantsIO.bySheetId(playerId, id)
    .map(_.map(grant => new DefaultSheetApi(ctx, playerId, grant)))

  override def create(request: CreateSheetRequest): Task[SheetCreationResponse] = {
    ctx.grantsIO.bySheetId(playerId, request.id).flatMap {
      case Some(_) => Task.raiseError(new Exception("YISSS it already exists"))
      case None => ctx.sheetsIO.init(SheetInit(id = request.id: SheetId,
        data = request.data: JsValue,
        author = playerId: UserId))
        .map(SheetCreationResponse.apply)
    }

  }
}
