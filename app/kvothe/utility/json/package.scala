package kvothe.utility


import kvothe.domain.SheetEntry
import kvothe.utility.tson.Tson
import kvothe.utility.tson.Tson.{ArrayOf, DictOf, Empty, ValOf}
import play.api.libs.json._

trait KvotheWriters{

  implicit def tsonWrites[T: Writes]: Writes[Tson[T]] = new Writes[Tson[T]] {self=>
    override def writes(tson: Tson[T]): JsValue = tson match {
      case ValOf(av) => implicitly[Writes[T]].writes(av)
      case Empty => JsNull
      case DictOf(map) => JsObject(map.mapValues(self.writes))
      case ArrayOf(seq) => JsArray(seq.map(self.writes))
    }
  }
  implicit val sheetEntryWrites: OWrites[SheetEntry] = Json.writes[SheetEntry]
}

package object json extends KvotheWriters {


}
