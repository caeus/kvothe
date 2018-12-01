package controllers

import scala.concurrent.{ExecutionContext, Future}

import engines.SheetsNode
import javax.inject.Inject
import kvothe.api.{PlayerApi, SheetCreationRequest}
import kvothe.pathd.PathdT
import kvothe.pathd.syntax._
import monix.eval.Task
import play.api.mvc._

class SheetsCtrl @Inject()(
  sheetsNode: SheetsNode,
  cc: ControllerComponents
)(implicit executionContext: ExecutionContext) extends AbstractController(cc) {

  def withUser[B](block: PlayerApi => B => Task[Result]): Request[AnyContent] => Future[Result]

  def entries = Action.async(withUser[AnyContent] { playerApi =>
    _ =>
      pathd("sheets", "entries")(playerApi.sheets.entries).swap.map(Ok(_))
  })


  def create = Action.async(withUser[SheetCreationRequest] { playerApi =>
    body =>
      pathd("sheets", "create")(playerApi.sheets.create(body)).swap.map(Ok(_))
  })


  def data(sheetId: String, versionId: String) = Action.async(withUser[AnyContent] {
    playerApi =>
      _ =>
        PathdT(pathd("sheets", "one")(playerApi.sheets.one(sheetId)).swap).flatMap {
          maybeSheet =>
            ???
        }
        ???
  })

  def changelog(sheetId: String, versionId: String) = Action.async(withUser[AnyContent] {
    playerApi =>
      _ =>
        playerApi.sheets.one(sheetId).map {
          entries =>
            Ok(Map("sheets" -> Map("entries" -> entries)))
        }
  })

  def versionsOf(sheetId: String) = Action.async { request =>
    ???
  }

  def update = Action.async { request =>
    ???
  }


}
