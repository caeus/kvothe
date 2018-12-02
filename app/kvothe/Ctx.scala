package kvothe

import javax.inject.{Inject, Singleton}
import repos.{GrantsIO, SheetsIO}

@Singleton
class Ctx @Inject()(val sheetsIO: SheetsIO, val grantsIO: GrantsIO)