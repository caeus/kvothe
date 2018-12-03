package kvothe

import javax.inject.{Inject, Singleton}
import depots.{GrantsDepot, SheetsDepot}

@Singleton
class Ctx @Inject()(val sheetsIO: SheetsDepot, val grantsIO: GrantsDepot)