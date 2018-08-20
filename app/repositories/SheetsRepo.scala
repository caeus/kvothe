package repositories


import scala.reflect.ClassTag

import akka.util.ByteString
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.Singleton
import model._
import monix.eval.Task
import play.api.libs.json.{JsObject, Json}
import utility.git.GitFileSystem
import utility.git.types.CommitRef
import utility.git.types.cmds._

@ImplementedBy(classOf[GitSheetsRepo])
trait SheetsRepo {
  def revisionsOf(sheetId: SheetId): Task[Seq[Revision]]
  def byId(
    sheetId: SheetId,
    revisionId: Option[RevisionId]
  ): Task[Option[Sheet]]
  def create(sheetId: SheetId, data: JsObject, byUserId: UserId): Task[Sheet]
  def revise(sheetId: SheetId, value: RevisionSeed, newData: JsObject): Task[Revision]
}
@Singleton
class GitSheetsRepo @Inject()(gitRepo: GitFileSystem) extends SheetsRepo {


  private def withLazyReader[T: ClassTag](of: SheetId, onBranch: String = "master")
    (block: Option[GitTx[ByteString]] => GitTx[T]): Task[T] = {
    gitRepo.exec(for {
      _ <- G.checkout(onBranch)
      exists <- F.exists(of.value)
      reader = if (exists) {
        Some(F.read(of.value))
      } else None
      r <- block(reader)
    } yield r)
  }

  private def toRevision(commitRef: CommitRef): Revision = Revision(id = RevisionId(commitRef.id),
    comment = commitRef.message,
    author = UserId(commitRef.author))


  def revisionsOf(sheetId: SheetId): Task[Seq[Revision]] = withLazyReader[Option[Seq[Revision]]](sheetId) {
    case Some(_) =>
      G.logOf(sheetId.value).map(_.toList.map(toRevision)).map(Some(_))
    case None => pure(None)
  }.flatMap {
    case Some(value) => Task.pure(value)
    case None => Task.raiseError(new Exception("There's no sheet with that shit"))
  }


  def byId(
    sheetId: SheetId,
    revisionId: Option[RevisionId]
  ): Task[Option[Sheet]] = {

    withLazyReader[Option[Sheet]](sheetId, revisionId.map(_.value).getOrElse("master")) {
      case Some(reader) =>
        for {
          content <- reader
          revisionId <- revisionId match {
            case Some(id) => pure(id)
            case None => G.logOf(sheetId.value)
              .map(_.take(1).toList.head.id)
              .map(RevisionId)
          }
        } yield Some(Sheet(sheetId,
          Json.parse(content.toArray)
            .as[JsObject], revisionId))
      case None => pure(None)
    }

  }

  def create(sheetId: SheetId, data: JsObject, byUserId: UserId): Task[Sheet] = {
    withLazyReader[Option[Sheet]](sheetId) {
      case Some(_) => pure(None)
      case None => for {
        _ <- G.checkout("master")
        _ <- F.touch(sheetId.value)
        _ <- F.write(sheetId.value, ByteString(Json.prettyPrint(data)))
        _ <- G.add(sheetId.value)
        _ <- G.commit(s"Start of sheet: $sheetId", byUserId.value)
        revisionId <- G.logOf(sheetId.value).map(_.take(1).toList.head.id)
      } yield {
        Some(Sheet(id = sheetId, data = data, revision = RevisionId(revisionId)))
      }
    }.flatMap {
      case Some(sheet) => Task.pure(sheet)
      case None => Task.raiseError(new Exception("Fix errors later"))
    }
  }


  def revise(sheetId: SheetId, value: RevisionSeed, newData: JsObject): Task[Revision] = {
    withLazyReader[Option[Revision]](sheetId) {
      case Some(reader) =>
        for {
          _ <- F.write(sheetId.value, ByteString(Json.prettyPrint(newData)))
          _ <- G.add(sheetId.value)
          _ <- G.commit(value.comment, value.author.value)
          revisionId <- G.logOf(sheetId.value).map(_.take(1).toSeq.head.id)
        } yield Some(Revision(RevisionId(revisionId), value.comment, value.author))
      case None => pure(None)
    }.flatMap {
      case Some(sheet) => Task.pure(sheet)
      case None => Task.raiseError(new Exception("Fix errors later"))
    }
  }

}
