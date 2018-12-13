package kvothe.api

import kvothe.Ctx
import kvothe.domain._
import monix.eval.Task

import scala.language.higherKinds

trait SheetApi {
  def versions: Task[Seq[SheetVersionEntry]]

  def versioned(version: Option[String]): Task[Option[VersionedSheetApi]]

  def update(request: UpdateSheetRequest): Task[SheetUpdateResponse]
}

class DefaultSheetApi(ctx: Ctx, playerId: UserId, grant: Grant) extends SheetApi {
  override def versions: Task[Seq[SheetVersionEntry]] = ctx.sheetsIO.versions(grant.sheetId)

  override def versioned(version: Option[String]): Task[Option[VersionedSheetApi]] =
    ctx.sheetsIO.versioned(grant.sheetId,version.map(SheetVersionId.apply)).map { v =>
      v.map(x => new DefaultVersionedSheetApi(ctx, grant, x))
    }

  override def update(request: UpdateSheetRequest): Task[SheetUpdateResponse] = {
    ctx.sheetsIO.update(grant.sheetId,SheetPatch(data = request.data,
      comment = request.comment,
      author = playerId)).map {
      entry: SheetVersionEntry =>
        SheetUpdateResponse(entry.id, entry.comment, entry.author)
    }
  }
}

