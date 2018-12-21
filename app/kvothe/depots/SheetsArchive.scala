package kvothe.depots

import scala.reflect.ClassTag

import akka.util.ByteString
import cats.data.Kleisli
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.{Named, Singleton}
import kvothe.domain._
import kvothe.utility.git.GitExecutor
import kvothe.utility.git.types.CommitRef
import kvothe.utility.git.types.cmds._
import monix.eval.Task
import play.api.libs.json.Json


@ImplementedBy(classOf[SheetsGitArchive])
trait SheetsArchive {

  def versions(sheetId: SheetId): Task[Seq[SheetVersionEntry]]

  def versioned(
    sheetId: SheetId,
    versionId: Option[SheetVersionId] = None
  ): Task[Option[VersionedSheet]]

  def init(seed: SheetInit): Task[Boolean]

  def update(sheetId: SheetId, patch: SheetPatch): Task[SheetVersionEntry]

  def changelog(sheetId: SheetId, version: SheetVersionId): Task[Option[SheetPatch]]
}


@Singleton
class SheetsGitArchive @Inject()(@Named("sheets") gitRepo: GitExecutor) extends SheetsArchive {


  private def toVersion(commitRef: CommitRef): SheetVersionEntry = SheetVersionEntry(id = SheetVersionId(commitRef.id),
    comment = commitRef.message,
    author = UserId(commitRef.author))

  override def versions(sheetId: SheetId): Task[Seq[SheetVersionEntry]] =
    withLazyContent[Task[Seq[SheetVersionEntry]]](sheetId) {
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

  Kleisli

  override def versioned(
    sheetId: SheetId,
    versionId: Option[SheetVersionId]
  ): Task[Option[VersionedSheet]] = {
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
          Json.parse(content.toArray), versionId))
      case None => pure(None) //TODO WTF????? what if version doesn't exist?
    }
  }

  override def update(
    sheetId: SheetId,
    patch: SheetPatch
  ): Task[SheetVersionEntry] = withLazyContent[SheetVersionEntry](sheetId) {
    case Some(reader) =>
      for {
        content <- reader
        newData = patch.data(Json.parse(content.toArray))
        _ <- F.write(sheetId.value, ByteString(Json.prettyPrint(newData)))
        _ <- G.add(sheetId.value)
        _ <- G.commit(patch.comment, patch.author.value)
        revisionId <- G.logOf(sheetId.value).map(_.take(1).toList.head.id)
      } yield {
        SheetVersionEntry(SheetVersionId(revisionId), patch.comment, patch.author)
      }
    case None => pure(???) // TODO SAME SHIT!
  }

  override def changelog(
    sheetId: SheetId,
    version: SheetVersionId
  ): Task[Option[SheetPatch]] = ???
}


