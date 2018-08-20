package repositories

import java.nio.file.{Files, Path, Paths, SimpleFileVisitor}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import model.{RevisionSeed, SheetId, UserId}
import monix.eval.Task
import monix.execution.Scheduler
import org.eclipse.jgit.api.Git
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json.{JsObject, Json}
import play.api.test._

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  *
  * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
  */
class GitSheetsRepoSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


  private implicit class SyncRunnable[T](task: Task[T]) {
    def unsafeRunSync(implicit executionContext: ExecutionContext): T = {
      implicit val scheduler: Scheduler = Scheduler(executionContext)
      task.runSyncUnsafe(20.seconds)
    }
  }

  override protected def beforeAll(): Unit = {

    Files.walkFileTree(Paths.get("internal/test"), new SimpleFileVisitor[Path] {

      import java.io.IOException
      import java.nio.file.attribute.BasicFileAttributes
      import java.nio.file.{FileVisitResult, Files}

      @throws[IOException]
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      @throws[IOException]
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
    Files.createDirectory(Paths.get("internal/test"))

    val repo = Git.init().setDirectory(Paths.get("internal/test").toFile).call()
    repo.commit().setAll(true).setMessage("Initial commit for test").call()
  }

  "GitSheetsRepo" should {

    "Create empty sheet" in {
      implicit val executionContext = inject[ExecutionContext]
      inject[SheetsRepo].create(SheetId("ezra"), JsObject.empty, UserId("me")).unsafeRunSync
      a[Exception] mustBe thrownBy {
        inject[SheetsRepo].create(SheetId("ezra"), JsObject.empty, UserId("me")).unsafeRunSync
      }

    }

    "Get sheet by Id" in {

      implicit val executionContext = inject[ExecutionContext]
      val sheet = inject[SheetsRepo].byId(SheetId("ezra"), None).unsafeRunSync

      sheet.get.data mustEqual JsObject.empty


      inject[SheetsRepo].byId(SheetId("lorenz"), None).unsafeRunSync mustEqual None


    }


    "Get revisions of sheet" in {
      implicit val executionContext = inject[ExecutionContext]
      val revisions = inject[SheetsRepo].revisionsOf(SheetId("ezra")).unsafeRunSync
      revisions.length mustBe 1

      revisions.head.author mustEqual UserId("me")

      a[Exception] mustBe thrownBy {
        inject[SheetsRepo].revisionsOf(SheetId("lorenz")).unsafeRunSync
      }

      //
    }

    "Revise sheet" in {

      implicit val executionContext = inject[ExecutionContext]
      val revision = inject[SheetsRepo].revise(SheetId("ezra"),
        RevisionSeed(comment = "Set name", author = UserId("me")),
        Json.obj("name" -> "ezra")).unsafeRunSync
      val revisions = inject[SheetsRepo].revisionsOf(SheetId("ezra")).unsafeRunSync

      revisions.head mustEqual revision
      inject[SheetsRepo].byId(SheetId("ezra"), None)
        .unsafeRunSync.get.data.value("name").as[String] mustEqual "ezra"

      inject[SheetsRepo].byId(SheetId("ezra"),
        Some(revisions.tail.head.id)).unsafeRunSync.get.data mustEqual JsObject.empty


    }
  }
}

case class GitSheetsRepoTestDeps()
