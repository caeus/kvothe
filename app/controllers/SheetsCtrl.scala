package controllers

import scala.concurrent.Future
import scala.language.higherKinds

import javax.inject.Inject
import kvothe.Ctx
import kvothe.api.PlayerApi
import kvothe.domain.{SheetCreationRequest, SheetId, SheetUpdateRequest, UserId}
import kvothe.utility.tson.TCursor
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.Json
import play.api.mvc._

class SheetsCtrl @Inject()(
  ctx: Ctx,
  cc: ControllerComponents
)(implicit scheduler: Scheduler) extends AbstractController(cc) {


  def withUser[B](block: PlayerApi => B => Task[Result]): Request[AnyContent] => Future[Result] = ???


  //  withUser{api => _ =>
  //    val asd = Chain[Task,PlayerApi[Task]](api).down("sheets")(_.sheets)
  //    asd.down("")(_.entries)
  //    ???
  //  }

  val playerId = UserId("caeus")


  def entries = Action.async {
    req =>
      TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
        .down("sheets")(api => Task.eval(api.sheets))
        .downArr("entries")(_.entries)
        .fold.runAsync
        .map(t => Ok(Json.toJson(t)))

  }


  def create = Action.async(parse.json[SheetCreationRequest]) {
    req =>
      TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
        .down("sheets")(api => Task.eval(api.sheets))
        .down("create")(_.create(req.body))
        .fold.runAsync
        .map(t => Ok(Json.toJson(t)))
  }


  def data(sheetId: String, versionId: String) = Action.async { _ =>
    TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
      .down("sheets")(api => Task.eval(api.sheets))
      .downOpt("one")(_.one(SheetId(sheetId)))
      .downOpt("versioned")(_.versioned(Some(versionId).filter(_ != "current")))
      .down("data")(_.data)
      .fold.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def changelog(sheetId: String, versionId: String) = Action.async { _ =>
    TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
      .down("sheets")(api => Task.eval(api.sheets))
      .downOpt("one")(_.one(SheetId(sheetId)))
      .downOpt("versioned")(_.versioned(Some(versionId).filter(_ != "current")))
      .down("changelog")(_.changelog)
      .fold.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def versionsOf(sheetId: String) = Action.async { request =>
    TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
      .down("sheets")(api => Task.eval(api.sheets))
      .downOpt("one")(_.one(SheetId(sheetId)))
      .down("versions")(_.versions)
      .fold.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def update(sheetId: String) = Action.async(parse.json[SheetUpdateRequest]) { request =>
    TCursor.from[Task, PlayerApi](PlayerApi(ctx, playerId))
      .down("sheets")(api => Task.eval(api.sheets))
      .downOpt("one")(_.one(SheetId(sheetId)))
      .down("update")(_.update(request.body))
      .fold.runAsync
      .map(t => Ok(Json.toJson(t)))
  }


}
