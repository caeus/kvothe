package repositories

import scala.reflect.ClassTag

import akka.util.ByteString
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.{Named, Singleton}
import model._
import monix.eval.Task
import play.api.libs.json.{JsObject, Json}
import utility.git.GitExecutor
import utility.git.types.CommitRef
import utility.git.types.cmds._


@ImplementedBy(classOf[SheetsGitIO])
trait SheetsIO {


  def versions(sheetId: SheetId): Task[Seq[SheetVersion]]

  def versioned(sheetId: SheetId)
    (versionId: Option[SheetVersionId] = None): Task[Option[VersionedSheet]]

  def init(seed: SheetInit): Task[Boolean]

  def patchSheet(sheetId: SheetId)(patch: SheetPatch2): Task[SheetVersion]

  def sheetPatch(sheetId: SheetId)(version: SheetVersionId): Task[Option[SheetPatch2]]
}


@Singleton
class SheetsGitIO @Inject()(@Named("sheets")gitRepo: GitExecutor) extends SheetsIO {


  private def toVersion(commitRef: CommitRef): SheetVersion = SheetVersion(id = SheetVersionId(commitRef.id),
    comment = commitRef.message,
    author = UserId(commitRef.author))

  override def versions(sheetId: SheetId): Task[Seq[SheetVersion]] =
    withLazyContent[Task[Seq[SheetVersion]]](sheetId) {
      case Some(_) =>
        G.logOf(sheetId.value).map(_.toList.map(toVersion))
          .map(Task.pure)
      case None =>
        pure(Task.raiseError(new Exception("NOOOOOO!!!!")))
    }.flatMap(identity)


  override def init(init: SheetInit): Task[Boolean] = {
    val sheetId = init.id
    withLazyContent[Boolean](init.id) {
      case Some(_) => pure(false)
      case None => for {
        _ <- G.checkout("master")
        _ <- F.touch(sheetId.value)
        _ <- F.write(sheetId.value, ByteString(Json.prettyPrint(init.data)))
        _ <- G.add(sheetId.value)
        _ <- G.commit(s"Start of sheet: $sheetId", init.author.value)
        revisionId <- G.logOf(sheetId.value).map(_.take(1).toList.head.id)
      } yield true

    }
  }


  private def withLazyContent[T: ClassTag](sheetId: SheetId, onBranch: String = "master")
    (block: Option[GitTx[ByteString]] => GitTx[T]): Task[T] = {
    gitRepo.exec(for {
      _ <- G.checkout(onBranch)
      exists <- F.exists(sheetId.value)
      reader = if (exists) {
        Some(F.read(sheetId.value))
      } else None
      r <- block(reader)
    } yield r)
  }


  override def versioned(sheetId: SheetId)
    (versionId: Option[SheetVersionId]): Task[Option[VersionedSheet]] = {
    withLazyContent[Option[VersionedSheet]](sheetId, versionId.map(_.value).getOrElse("master")) {
      case Some(reader) =>
        for {
          content <- reader
          versionId <- versionId match {
            case Some(value) => pure(value)
            case None => G.logOf(sheetId.value)
              .map(_.take(1).toList.head.id)
              .map(SheetVersionId)
          }
        } yield Some(VersionedSheet(sheetId,
          Json.parse(content.toArray)
            .as[JsObject], versionId))

    }
  }

  override def patchSheet(sheetId: SheetId)
    (patch: SheetPatch2): Task[SheetVersion] = withLazyContent[SheetVersion](sheetId) {
    case Some(reader) =>
      for {
        content <- reader
        newData = patch.data(Json.parse(content.toArray))
        _ <- F.write(sheetId.value, ByteString(Json.prettyPrint(newData)))
        _ <- G.add(sheetId.value)
        _ <- G.commit(patch.comment, patch.author.value)
        revisionId <- G.logOf(sheetId.value).map(_.take(1).toList.head.id)
      } yield {
        SheetVersion(SheetVersionId(revisionId), patch.comment, patch.author)
      }
  }

  override def sheetPatch(sheetId: SheetId)
    (version: SheetVersionId): Task[Option[SheetPatch2]] = ???
}


