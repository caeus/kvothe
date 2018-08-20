package model

import play.api.libs.json.JsObject


case class Sheet(
  id: SheetId,
  data: JsObject,
  revision: RevisionId // sure?
)

case class SheetRef(
  id: SheetId
)

case class Revision(
  id: RevisionId,
  comment: String,
  author: UserId
)
case class RevisionSeed(
  comment: String,
  author: UserId
)
case class SheetId(value: String) extends AnyVal
case class RevisionId(value: String) extends AnyVal
case class UserId(value: String) extends AnyVal


case class UserSheet(userId: UserId, sheetId: SheetId)