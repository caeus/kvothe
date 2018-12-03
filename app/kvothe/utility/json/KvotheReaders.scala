package kvothe.utility.json

import cats.MonadError
import kvothe.domain._
import play.api.libs.json._
import gnieh.diffson.playJson.DiffsonProtocol._
trait KvotheReaders{

  implicit val sheetIdReader = Reads.of[String].map(SheetId)
  implicit val sheetCreationRequestReader = Json.reads[SheetCreationRequest]
  implicit val sheetUpdateRequestReader = Json.reads[SheetUpdateRequest]
}
