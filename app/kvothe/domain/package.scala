package kvothe

import scala.language.higherKinds

import play.api.libs.json.JsValue

package object domain {

  case class SheetVersionEntry(
    id: SheetVersionId,
    comment: String,
    author: UserId
  )

  case class VersionChangelog(id:String/*todo add smth relevant*/)

  import gnieh.diffson.playJson.JsonPatch

  case class SheetUpdateRequest(
    data: JsonPatch,
    comment: String
  )

  case class SheetUpdateResponse(
    id: SheetVersionId,
    comment: String,
    author: UserId
  )


  case class SheetEntry(id: String)

  case class SheetCreationRequest(id: SheetId, data: JsValue)

  case class SheetCreationResponse(success:Boolean)

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


  case class SheetPatch(
    data: JsonPatch,
    comment: String,
    author: UserId
  )

  case class SheetId(value: String) extends AnyVal

  case class SheetVersionId(value: String) extends AnyVal

  case class UserId(value: String) extends AnyVal


  case class Grant(userId: UserId, sheetId: SheetId)

}
