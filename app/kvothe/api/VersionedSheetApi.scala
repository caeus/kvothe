package kvothe.api

import kvothe.Ctx
import kvothe.domain.{Grant, VersionChangelog, VersionedSheet}
import monix.eval.Task
import play.api.libs.json.JsValue

trait VersionedSheetApi {

  def changelog: Task[VersionChangelog]

  def data: Task[JsValue]

}

class DefaultVersionedSheetApi(ctx: Ctx,
                               grant: Grant,
                               versionedSheet: VersionedSheet
                              ) extends VersionedSheetApi {
  override def changelog: Task[VersionChangelog] = Task.pure(VersionChangelog(""))

  override def data: Task[JsValue] = Task.pure(versionedSheet.data)
}