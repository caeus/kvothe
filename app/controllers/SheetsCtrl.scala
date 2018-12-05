package controllers

import scala.concurrent.Future
import scala.language.higherKinds

import javax.inject.Inject
import kvothe.Ctx
import kvothe.api.PlayerApi
import kvothe.domain.{SheetCreationRequest, SheetId, SheetUpdateRequest, UserId}
import kvothe.utility.tson.TPot
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
      TPot.pure[Task](PlayerApi(ctx, playerId))
        .growVal("sheets")(api => Task.eval(api.sheets))
        .growSeq("entries")(_.entries)
        .collapse.runAsync
        .map(t => Ok(Json.toJson(t)))

  }


  def create = Action.async(parse.json[SheetCreationRequest]) {
    req =>
      TPot.pure[Task](PlayerApi(ctx, playerId))
        .growVal("sheets")(api => Task.eval(api.sheets))
        .growVal("create")(_.create(req.body))
        .collapse.runAsync
        .map(t => Ok(Json.toJson(t)))
  }


  def data(sheetId: String, versionId: String) = Action.async { _ =>
    TPot.pure[Task](PlayerApi(ctx, playerId))
      .growVal("sheets")(api => Task.eval(api.sheets))
      .growOpt("one")(_.one(SheetId(sheetId)))
      .growOpt("versioned")(_.versioned(Some(versionId).filter(_ != "current")))
      .growVal("data")(_.data)
      .collapse.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def changelog(sheetId: String, versionId: String) = Action.async { _ =>
    TPot.pure[Task](PlayerApi(ctx, playerId))
      .growVal("sheets")(api => Task.eval(api.sheets))
      .growOpt("one")(_.one(SheetId(sheetId)))
      .growOpt("versioned")(_.versioned(Some(versionId).filter(_ != "current")))
      .growVal("changelog")(_.changelog)
      .collapse.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def versionsOf(sheetId: String) = Action.async { request =>
    TPot.pure[Task](PlayerApi(ctx, playerId))
      .growVal("sheets")(api => Task.eval(api.sheets))
      .growOpt("one")(_.one(SheetId(sheetId)))
      .growVal("versions")(_.versions)
      .collapse.runAsync
      .map(t => Ok(Json.toJson(t)))
  }

  def update(sheetId: String) = Action.async(parse.json[SheetUpdateRequest]) { request =>
    TPot.pure[Task](PlayerApi(ctx, playerId))
      .growVal("sheets")(api => Task.eval(api.sheets))
      .growOpt("one")(_.one(SheetId(sheetId)))
      .growVal("update")(_.update(request.body))
      .collapse.runAsync
      .map(t => Ok(Json.toJson(t)))
  }


}
