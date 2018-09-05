package controllers

import scala.concurrent.ExecutionContext

import engines.SheetsNode
import javax.inject.Inject
import model.SheetId
import play.api.mvc._

class SheetsCtrl @Inject()(
  sheetsNode: SheetsNode,
  cc: ControllerComponents
)(implicit executionContext: ExecutionContext) extends AbstractController(cc) {



  def all() = play.mvc.Results.TODO

  def init() = play.mvc.Results.TODO

  def versionsOf(sheetId: String) = play.mvc.Results.TODO

  def versioned(sheetId: String, versionId: Option[String]) = play.mvc.Results.TODO

  def patch(sheetId: String) = Action.withSession {
    req =>
      implicit userId =>
        sheetsNode.patch(SheetId(sheetId))(null)
        ???
  }


}
object FindBetterName {


}
