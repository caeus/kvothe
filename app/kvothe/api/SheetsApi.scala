package kvothe.api

import monix.eval.Task

trait SheetsApi {
  def entries: Task[Seq[SheetEntry]]
  def one(id: String): Task[Option[SheetApi]]
  def create(request: SheetCreationRequest): Task[SheetCreationResponse]
}
class DefaultSheetsApi(playerId:String) extends SheetsApi {
  override def entries: Task[Seq[SheetEntry]] = ???

  override def one(id: String): Task[Option[SheetApi]] = ???

  override def create(request: SheetCreationRequest): Task[SheetCreationResponse] = ???
}
