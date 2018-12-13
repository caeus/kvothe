package kvothe.utility.json

import edu.caeus.herbivicus.sower.Pipe
import edu.caeus.herbivicus.vine.Vine
import edu.caeus.herbivicus.vine.Vine.PartialFolder
import kvothe.domain._
import play.api.libs.json._

trait KvotheWriters {

  implicit def tsonWrites[T](implicit T: Writes[T]): Writes[Vine[T]] = new Writes[Vine[T]] {
    self =>
    override def writes(tson: Vine[T]): JsValue = tson.fold(
      JsNull,
      av => T.writes(av),
      seq => JsArray(seq.map(self.writes)),
      map => JsObject(map.mapValues(self.writes))
    )
  }

  implicit def pipeWriter[T](implicit writes: Writes[Vine[T]]): Pipe[Vine[T], JsValue] =
    Pipe[Vine[T], JsValue](writes.writes)

  import play.api.libs.json._

  def vinePartialFolderJsValue: Vine.PartialFolder[JsValue] = PartialFolder[JsValue](JsNull,
    array => JsArray(array),
    dict => JsObject(dict))

  implicit val userIdWrites: Writes[UserId] = Writes[UserId](id => JsString(id.value))
  implicit val sheetEntryWrites: OWrites[SheetEntry] = Json.writes[SheetEntry]
  implicit val sheetCreationResponseWrites: OWrites[SheetCreationResponse] = Json.writes[SheetCreationResponse]
  implicit val versionChangelogWrites: OWrites[VersionChangelog] = Json.writes[VersionChangelog]
  implicit val sheetVersionEntryWrites: OWrites[SheetVersionEntry] = Json.writes[SheetVersionEntry]
  implicit val sheetVersionIdWrites: Writes[SheetVersionId] = Writes[SheetVersionId](id => JsString(id.value))
  implicit val sheetUpdateResponseWrites: OWrites[SheetUpdateResponse] = Json.writes[SheetUpdateResponse]
}
