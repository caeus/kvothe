package kvothe.utility.json

import kvothe.domain._
import play.api.libs.json._
import gnieh.diffson.playJson.provider._

trait KvotheReaders {
  
  implicit val sheetIdReader: Reads[SheetId] = Reads.of[String].map(SheetId)
  implicit val sheetCreationRequestReader: Reads[CreateSheetRequest] = Json.reads[CreateSheetRequest]
  implicit val sheetUpdateRequestReader: Reads[UpdateSheetRequest] = Json.reads[UpdateSheetRequest]
}
