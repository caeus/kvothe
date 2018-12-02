package kvothe.repos

import java.nio.file.Paths

import gnieh.diffson.Part
import kvothe.domain._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json.{JsObject, JsString}
import play.api.test._

import scala.collection.immutable
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  *
  * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
  *
  */
class SheetsGitIOSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


  val testSheetsRepoRoot = Paths.get("internal/test/sheets")

  private implicit class SyncRunnable[T](task: Task[T]) {
    def unsafeRunSync(implicit executionContext: Scheduler): T = {
      task.runSyncUnsafe(20.seconds)
    }
  }

  override protected def beforeAll(): Unit = {
    RestartRepo(testSheetsRepoRoot)
  }

  "GitSheetsRepo" should {

    "Create empty sheet" in {
      implicit val executionContext = inject[Scheduler]
      inject[SheetsIO].init(SheetInit(SheetId("ezra"), JsObject.empty, UserId("me"))).unsafeRunSync

      inject[SheetsIO].init(SheetInit(SheetId("ezra"), JsObject.empty, UserId("me"))).unsafeRunSync mustBe false


    }

    "Get sheet by Id" in {

      implicit val executionContext = inject[Scheduler]
      val sheet = inject[SheetsIO].versioned(SheetId("ezra"))(None).unsafeRunSync

      sheet.get.data mustEqual JsObject.empty

      a[Exception] mustBe thrownBy {
        inject[SheetsIO].versioned(SheetId("lorenz"))(None).unsafeRunSync mustBe false
      }
    }


    "Get revisions of sheet" in {
      implicit val executionContext = inject[Scheduler]
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
      implicit val executionContext = inject[Scheduler]
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


