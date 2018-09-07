package engines


import javax.inject.Inject
import model._
import monix.eval.Task
import repositories.{SheetsIO, GrantsIO}


class SheetsNode @Inject()(
  sheetsIO: SheetsIO,
  grantsIO: GrantsIO
) {


  def all(implicit userId: UserId): Task[Seq[SheetRef]] = {
    grantsIO.allOf(userId)
      .map(_.map {
        case Grant(_, sheetId) => SheetRef(sheetId)
      })
  }


  def versionsOf(sheetId: SheetId)(implicit userId: UserId): Task[Seq[SheetVersion]] = {
    grantsIO.bySheetId(userId, sheetId)
      .otherwise(new Exception("FIXXX")).flatMap { _ =>
      sheetsIO.versions(sheetId)
    }
  }

  def versioned(sheetId: SheetId)(versionId: Option[SheetVersionId])
    (implicit userId: UserId): Task[Option[VersionedSheet]] = {
    grantsIO.bySheetId(userId, sheetId)
      .otherwise(new Exception("Fixxx2")).flatMap {
      _ =>
        sheetsIO.versioned(sheetId)(versionId)
    }
  }

  def patch(sheetId: SheetId)(patch: SheetPatch)
    (implicit userId: UserId): Task[SheetVersion] = {
    grantsIO.bySheetId(userId, sheetId)
      .otherwise(new Exception("Fixxxx4"))
      .flatMap {
        _ => ???
      }

  }


}
