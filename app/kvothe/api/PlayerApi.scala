package kvothe.api

import kvothe.Ctx
import kvothe.domain.UserId
import monix.eval.Task

import scala.language.higherKinds

trait PlayerApi {
  def sheets: SheetsApi
}
class DefaultPlayerApi(ctx:Ctx,id:UserId) extends PlayerApi {

  override def sheets: SheetsApi = new DefaultSheetsApi(ctx,id)

}
