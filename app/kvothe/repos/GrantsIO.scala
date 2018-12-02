package kvothe.repos


import com.google.inject.{ImplementedBy, Inject}
import javax.inject.{Named, Singleton}
import kvothe.domain.{Grant, SheetId, UserId}
import kvothe.utility.git.GitExecutor
import monix.eval.Task

@ImplementedBy(classOf[GrantsGitIO])
trait GrantsIO {

  def allOf(userId: UserId): Task[Seq[Grant]]

  def bySheetId(userId: UserId, sheetId: SheetId): Task[Option[Grant]]

  def grantTo(userId: UserId, sheetId: SheetId): Task[Boolean]

}

@Singleton
class GrantsGitIO @Inject()(@Named("grants") executor: GitExecutor) extends GrantsIO {
  import kvothe.utility.git.types.cmds._

  override def allOf(userId: UserId): Task[Seq[Grant]] = {
    executor exec (for {
      _ <- G.checkout("master")
      paths <- F.listFiles(userId.value)
    } yield {
      paths.map {
        path =>
          Grant(userId = userId,
            sheetId = SheetId(path.getFileName.toString))
      }
    })
  }

  override def bySheetId(
    userId: UserId,
    sheetId: SheetId
  ): Task[Option[Grant]] = executor exec (for {
    _ <- G.checkout("master")
    exists <- F.exists(s"${userId.value}/${sheetId.value}")
  } yield {
    if (exists) Some(Grant(userId, sheetId)) else None
  })

  override def grantTo(
    userId: UserId,
    sheetId: SheetId
  ): Task[Boolean] = {
    executor exec (for {
      _ <- G.checkout("master")
      exists <- F.exists(s"${userId.value}/${sheetId.value}")
      _ = println(s"WTF $exists = ${userId.value}/${sheetId.value}")
      _ <- if (!exists) F.touch(s"${userId.value}/${sheetId.value}")
      else pure(())
    } yield
      !exists
    )
  }
}