package controllers

import javax.inject.Inject
import kvothe.Ctx
import kvothe.api.{DefaultPlayerApi, PlayerApi}
import kvothe.domain.UserId
import kvothe.utility.tson.TCursor
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.circe.Circe
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

class SheetsCtrl @Inject()(
                            ctx: Ctx,
                            cc: ControllerComponents
                          )(implicit scheduler: Scheduler) extends AbstractController(cc)
  with Circe {


  def withUser[B](block: PlayerApi => B => Task[Result]): Request[AnyContent] => Future[Result] = ???


  //  withUser{api => _ =>
  //    val asd = Chain[Task,PlayerApi[Task]](api).down("sheets")(_.sheets)
  //    asd.down("")(_.entries)
  //    ???
  //  }

  val playerId = UserId("caeus")


  def entries = Action.async {
    req =>

      TCursor.from[Task, PlayerApi](new DefaultPlayerApi(ctx, playerId))
        .down("sheets")(api => Task.eval(api.sheets))
        .downArr("entries")(_.entries)
        .fold.runAsync
        .map(t => Ok(Json.toJson(t)))

  }


  def create = Action.async(_ => ???)


  def data(sheetId: String, versionId: String) = Action.async(_ => ???)

  def changelog(sheetId: String, versionId: String) = Action.async(_ => ???)

  def versionsOf(sheetId: String) = Action.async { request =>
    ???
  }

  def update(sheetId: String) = Action.async { request =>
    ???
  }


}
