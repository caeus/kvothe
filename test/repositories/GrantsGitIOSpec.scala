package repositories

import java.nio.file.Paths

import scala.collection.immutable
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import gnieh.diffson.Part
import model._
import monix.eval.Task
import monix.execution.Scheduler
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
class GrantsGitIOSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


  val testGrantsRepoRoot = Paths.get("internal/test/grants")

  private implicit class SyncRunnable[T](task: Task[T]) {
    def unsafeRunSync(implicit executionContext: ExecutionContext): T = {
      implicit val scheduler: Scheduler = Scheduler(executionContext)
      task.runSyncUnsafe(20.seconds)
    }
  }

  override protected def beforeAll(): Unit = {
    RestartRepo(testGrantsRepoRoot)
  }

  "GrantsGitIO" should {

    "grant permission" in {
      implicit val executionContext = inject[ExecutionContext]


      inject[GrantsIO].bySheetId(UserId("caeus"), SheetId("ezra")).unsafeRunSync.isEmpty mustBe true
      inject[GrantsIO].grantTo(UserId("caeus"), SheetId("ezra")).unsafeRunSync

      inject[GrantsIO].bySheetId(UserId("caeus"), SheetId("ezra")).unsafeRunSync.isDefined mustBe true
      inject[GrantsIO].allOf(UserId("caeus")).unsafeRunSync.length mustBe 1

    }


  }
}


