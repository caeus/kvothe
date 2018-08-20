package engines


import model._
import monix.eval.Task
import repositories.{SheetsRepo, UserSheetsRepo}


class SheetsEngine(
  sheetsRepo: SheetsRepo,
  userSheetsRepo: UserSheetsRepo
) {


  def all(implicit userId: UserId): Task[Seq[SheetRef]] = {
    userSheetsRepo.allOf(userId)
      .map(_.map {
        case UserSheet(_, sheetId) => SheetRef(sheetId)
      })
  }


  def revisionsOf(sheetId: SheetId)(implicit userId: UserId): Task[Seq[Revision]] = {
    userSheetsRepo.bySheetId(userId, sheetId).flatMap {
      case None => ???
      case Some(_) => sheetsRepo.revisionsOf(sheetId)
    }

  }

  def byId(sheetId: SheetId, revisionId: Option[RevisionId])
    (implicit userId: UserId): Task[Option[Sheet]] = {
    userSheetsRepo.bySheetId(userId, sheetId).flatMap {
      case None => ???
      case Some(_) =>
        sheetsRepo.byId(sheetId, revisionId)
    }
  }

  def revise(sheetId: SheetId, revision: RevisionSeed)
    (implicit userId: UserId): Task[Revision] = {
    userSheetsRepo.bySheetId(userId, sheetId)
      .flatMap {
        case None => ???
        case Some(_) =>
          sheetsRepo.revise(sheetId, revision,null)
      }

  }


}
