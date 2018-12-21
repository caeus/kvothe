package kvothe

import javax.inject.{Inject, Singleton}
import depots.{GrantsArchive, SheetsArchive}

@Singleton
class Ctx @Inject()(val sheetsArchive: SheetsArchive, val grantsArchive: GrantsArchive)