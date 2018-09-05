package repositories

import java.nio.file.{Files, Path, Paths, SimpleFileVisitor}

import scala.collection.immutable
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import gnieh.diffson.Part
import model._
import monix.eval.Task
import monix.execution.Scheduler
import org.eclipse.jgit.api.Git
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json.{JsObject, JsString}
import play.api.test._

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  *
  * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
  *
  */
class SheetsGitIOSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


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
      inject[SheetsIO].init(SheetInit(SheetId("ezra"), JsObject.empty, UserId("me"))).unsafeRunSync

      inject[SheetsIO].init(SheetInit(SheetId("ezra"), JsObject.empty, UserId("me"))).unsafeRunSync mustBe false


    }

    "Get sheet by Id" in {

      implicit val executionContext = inject[ExecutionContext]
      val sheet = inject[SheetsIO].versioned(SheetId("ezra"))(None).unsafeRunSync

      sheet.get.data mustEqual JsObject.empty

      a[Exception] mustBe thrownBy {
        inject[SheetsIO].versioned(SheetId("lorenz"))(None).unsafeRunSync mustBe false
      }
    }


    "Get revisions of sheet" in {
      implicit val executionContext = inject[ExecutionContext]
      val revisions = inject[SheetsIO].versions(SheetId("ezra"))

        .unsafeRunSync
      revisions.length mustBe 1

      revisions.head.author mustEqual UserId("me")

      a[Exception] mustBe thrownBy {
        inject[SheetsIO].versions(SheetId("lorenz")).unsafeRunSync
      }

      //
    }

    "Patch sheet" in {
      import gnieh.diffson.playJson._
      val asd: immutable.Seq[Part] = JsonPointer(null).path
      implicit val executionContext = inject[ExecutionContext]
      val version = inject[SheetsIO].patchSheet(SheetId("ezra"))(
        SheetPatch2(data = JsonPatch(Add(Queue(Left("name")), JsString("ezra"))), comment = "Set name", author = UserId("me"))).unsafeRunSync
      val versions = inject[SheetsIO].versions(SheetId("ezra")).unsafeRunSync

      versions.head mustEqual version
      inject[SheetsIO].versioned(SheetId("ezra"))(None)
        .unsafeRunSync.get.data.as[JsObject].value("name").as[String] mustEqual "ezra"

      inject[SheetsIO].versioned(SheetId("ezra"))(Some(versions.tail.head.id))
        .unsafeRunSync.get.data mustEqual JsObject.empty


    }
  }
}

case class GitSheetsRepoTestDeps()
