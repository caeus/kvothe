package model

import play.api.libs.json.{JsArray, JsValue}
import gnieh.diffson.playJson.JsonPatch

case class VersionedSheet(
  id: SheetId,
  data: JsValue,
  version: SheetVersionId // sure?
)

case class SheetRef(
  id: SheetId
)

case class SheetInit(
  id: SheetId,
  data: JsValue,
  author: UserId
)

case class SheetVersion(
  id: SheetVersionId,
  comment: String,
  author: UserId
)


case class SheetPatch2(
  data: JsonPatch,
  comment: String,
  author: UserId
)

case class SheetId(value: String) extends AnyVal
case class SheetVersionId(value: String) extends AnyVal
case class UserId(value: String) extends AnyVal

case class SheetPatch(value:JsArray)



case class UserSheet(userId: UserId, sheetId: SheetId)