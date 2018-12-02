package kvothe

import play.api.libs.json.{JsArray, JsValue}

import scala.language.higherKinds

package object domain {

  case class SheetVersionEntry(
                                id: SheetVersionId,
                                comment: String,
                                author: UserId
                              )

  case class VersionChangelog()

  import gnieh.diffson.playJson.JsonPatch

  case class SheetUpdateRequest(
                                 data: JsonPatch,
                                 comment: String
                               )

  case class SheetUpdateResponse(id: SheetVersionId,
                                 comment: String,
                                 author: UserId)


  case class SheetEntry(id: String)

  trait SheetCreationRequest

  trait SheetCreationResponse

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


  case class SheetPatch2(
                          data: JsonPatch,
                          comment: String,
                          author: UserId
                        )

  case class SheetId(value: String) extends AnyVal

  case class SheetVersionId(value: String) extends AnyVal

  case class UserId(value: String) extends AnyVal

  case class SheetPatch(value: JsArray)


  case class Grant(userId: UserId, sheetId: SheetId)

}
