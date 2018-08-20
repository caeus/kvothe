package repositories


import model.{SheetId, UserId, UserSheet}
import monix.eval.Task

class UserSheetsRepo {

  def allOf(userId: UserId): Task[Seq[UserSheet]] = ???

  def bySheetId(userId:UserId,sheetId: SheetId):Task[Option[UserSheet]] = ???

}
