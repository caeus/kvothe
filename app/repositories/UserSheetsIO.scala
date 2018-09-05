package repositories


import com.google.inject.{ImplementedBy, Inject}
import javax.inject.Singleton
import model.{SheetId, UserId, UserSheet}
import monix.eval.Task
import utility.git.GitExecutor
@ImplementedBy(classOf[UserSheetsGitIO])
trait UserSheetsIO {

  def allOf(userId: UserId): Task[Seq[UserSheet]]

  def bySheetId(userId: UserId, sheetId: SheetId): Task[Option[UserSheet]]

}

@Singleton
class UserSheetsGitIO @Inject()(executor: GitExecutor) extends UserSheetsIO {
  import utility.git.types.cmds._

  override def allOf(userId: UserId): Task[Seq[UserSheet]] = {
    executor exec (for {
      _ <- G.checkout("master")
      paths <- F.listFiles(userId.value)
    } yield {
      paths.map {
        path =>
          UserSheet(userId = userId,
            sheetId = SheetId(path.getFileName.toString))
      }
    })
  }

  override def bySheetId(
    userId: UserId,
    sheetId: SheetId
  ): Task[Option[UserSheet]] = executor exec (for {
    _ <- G.checkout("master")
    exists <- F.exists(s"${userId.value}/${sheetId.value}")
  } yield {
    if (exists) Some(UserSheet(userId, sheetId)) else None
  })
}