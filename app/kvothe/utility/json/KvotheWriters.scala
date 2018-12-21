package kvothe.utility.json

import io.sower.Pipe
import io.vine.Vine
import kvothe.domain._
import play.api.libs.json._

trait KvotheWriters {

  implicit def tsonWrites[T](implicit T: Writes[T]): Writes[Vine[T]] = new Writes[Vine[T]] {
    self =>
    override def writes(tson: Vine[T]): JsValue = tson.map(T.writes).combine(
      JsNull,
      seq => JsArray(seq),
      map => JsObject(map)
    )
  }

  implicit def pipeWriter[T](implicit writes: Writes[Vine[T]]): Pipe[Vine[T], JsValue] =
    Pipe[Vine[T], JsValue](writes.writes)

  import play.api.libs.json._

  def vineOfJsValueCombiner: Vine.Combiner[JsValue] = Vine.Combiner(JsNull, JsArray(_), JsObject(_))

  implicit val userIdWrites: Writes[UserId] = Writes[UserId](id => JsString(id.value))
  implicit val sheetEntryWrites: OWrites[SheetEntry] = Json.writes[SheetEntry]
  implicit val sheetCreationResponseWrites: OWrites[SheetCreationResponse] = Json.writes[SheetCreationResponse]
  implicit val versionChangelogWrites: OWrites[VersionChangelog] = Json.writes[VersionChangelog]
  implicit val sheetVersionIdWrites: Writes[SheetVersionId] = Writes[SheetVersionId](id => JsString(id.value))

  implicit val sheetVersionEntryWrites: OWrites[SheetVersionEntry] = Json.writes[SheetVersionEntry]
  implicit val sheetUpdateResponseWrites: OWrites[SheetUpdateResponse] = Json.writes[SheetUpdateResponse]
  implicit val playerCompileError: OWrites[PlayerCompileError] = Json.writes[PlayerCompileError]
}
