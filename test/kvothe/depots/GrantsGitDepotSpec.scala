package kvothe.depots

import java.nio.file.Paths

import kvothe.domain._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  *
  * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
  *
  */
class GrantsGitDepotSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


  val testGrantsRepoRoot = Paths.get("internal/test/grants")

  private implicit class SyncRunnable[T](task: Task[T]) {
    def unsafeRunSync(implicit executionContext: Scheduler): T = {
      task.runSyncUnsafe(20.seconds)
    }
  }

  override protected def beforeAll(): Unit = {
    RestartRepo(testGrantsRepoRoot)
  }

  "GrantsGitIO" should {

    "grant permission" in {
      implicit val executionContext = inject[Scheduler]


      inject[GrantsDepot].bySheetId(UserId("caeus"), SheetId("ezra")).unsafeRunSync.isEmpty mustBe true
      inject[GrantsDepot].grantTo(UserId("caeus"), SheetId("ezra")).unsafeRunSync

      inject[GrantsDepot].bySheetId(UserId("caeus"), SheetId("ezra")).unsafeRunSync.isDefined mustBe true
      inject[GrantsDepot].allOf(UserId("caeus")).unsafeRunSync.length mustBe 1

    }


  }
}


