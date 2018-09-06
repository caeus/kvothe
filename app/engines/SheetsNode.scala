package engines


import javax.inject.Inject
import model._
import monix.eval.Task
import repositories.{SheetsIO, GrantsIO}


class SheetsNode @Inject()(
  sheetsRepo: SheetsIO,
  userSheetsRepo: GrantsIO
) {


  def all(implicit userId: UserId): Task[Seq[SheetRef]] = {
    userSheetsRepo.allOf(userId)
      .map(_.map {
        case Grant(_, sheetId) => SheetRef(sheetId)
      })
  }


  def versionsOf(sheetId: SheetId)(implicit userId: UserId): Task[Seq[SheetVersion]] = {
    userSheetsRepo.bySheetId(userId, sheetId)
      .otherwise(new Exception("FIXXX")).flatMap { _ =>
      sheetsRepo.versions(sheetId)
    }
  }

  def versioned(sheetId: SheetId)(versionId: Option[SheetVersionId])
    (implicit userId: UserId): Task[Option[VersionedSheet]] = {
    userSheetsRepo.bySheetId(userId, sheetId)
      .otherwise(new Exception("Fixxx2")).flatMap {
      _ =>
        sheetsRepo.versioned(sheetId)()
    }
  }

  def patch(sheetId: SheetId)(patch: SheetPatch)
    (implicit userId: UserId): Task[SheetVersion] = {
    userSheetsRepo.bySheetId(userId, sheetId)
      .otherwise(new Exception("Fixxxx4"))
      .flatMap {
        _ => ???
      }

  }


}
