package kvothe.utility.json

import kvothe.domain._
import kvothe.utility.tson.Tson
import kvothe.utility.tson.Tson.{ArrayOf, DictOf, Empty, ValOf}
import play.api.libs.json._

trait KvotheReaders{

  implicit val sheetCreationRequestReader = Json.reads[SheetCreationRequest]
  implicit val sheetUpdateRequestReader = Json.reads[SheetUpdateRequest]
}
