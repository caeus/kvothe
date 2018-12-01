package kvothe

import monix.eval.Task

package object api {

  trait SheetVersionEntry

  trait VersionChangelog

  trait SheetData

  trait SheetUpdateRequest

  trait SheetUpdateResponse

  trait VersionedSheetApi {
    def changelog: Task[VersionChangelog]
    def data: Task[SheetData]
  }

  trait SheetApi {
    def versions: Task[Seq[SheetVersionEntry]]
    def versioned(version: Option[String]): Task[Option[VersionedSheetApi]]
    def update(request: SheetUpdateRequest): Task[SheetUpdateResponse]
  }

  trait SheetEntry

  trait SheetCreationRequest

  trait SheetCreationResponse

}
