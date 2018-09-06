package config

import com.google.inject.AbstractModule
import com.google.inject.name.Names
import utility.git.{GitExecutor, GrantsGitExecutor, SheetsGitExecutor}

class KvthModule extends AbstractModule {
  def configure() = {

    bind(classOf[GitExecutor])
      .annotatedWith(Names.named("sheets"))
      .to(classOf[SheetsGitExecutor])

    bind(classOf[GitExecutor])
      .annotatedWith(Names.named("grants"))
      .to(classOf[GrantsGitExecutor])
  }
}