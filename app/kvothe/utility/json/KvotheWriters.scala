package kvothe.utility.json

import kvothe.domain._
import kvothe.rost.Pipe
import kvothe.utility.tson.Tson
import kvothe.utility.tson.Tson.{ArrayOf, DictOf, Empty, ValOf}
import play.api.libs.json._

trait KvotheWriters {

  implicit def tsonWrites[T](implicit T: Writes[T]): Writes[Tson[T]] = new Writes[Tson[T]] {
    self =>
    override def writes(tson: Tson[T]): JsValue = tson.fold(
      JsNull,
      av => T.writes(av),
      seq => JsArray(seq.map(self.writes)),
      map => JsObject(map.mapValues(self.writes))
    )
  }

  implicit def pipeWriter[T](implicit writes: Writes[Tson[T]]): Pipe[Tson[T], JsValue] =
    Pipe[Tson[T], JsValue](writes.writes)

  import play.api.libs.json._

  implicit val userIdWrites: Writes[UserId] = Writes[UserId](id => JsString(id.value))
  implicit val sheetEntryWrites: OWrites[SheetEntry] = Json.writes[SheetEntry]
  implicit val sheetCreationResponseWrites: OWrites[SheetCreationResponse] = Json.writes[SheetCreationResponse]
  implicit val versionChangelogWrites: OWrites[VersionChangelog] = Json.writes[VersionChangelog]
  implicit val sheetVersionEntryWrites: OWrites[SheetVersionEntry] = Json.writes[SheetVersionEntry]
  implicit val sheetVersionIdWrites: Writes[SheetVersionId] = Writes[SheetVersionId](id => JsString(id.value))
  implicit val sheetUpdateResponseWrites: OWrites[SheetUpdateResponse] = Json.writes[SheetUpdateResponse]
}
