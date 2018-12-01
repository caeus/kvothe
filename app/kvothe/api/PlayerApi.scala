package kvothe.api

trait PlayerApi {
  def sheets: SheetsApi
}
class DefaultPlayerApi(id: String) extends PlayerApi {

  override def sheets: SheetsApi = new DefaultSheetsApi(id)

}
