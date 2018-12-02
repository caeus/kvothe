package config

import com.google.inject.AbstractModule
import com.google.inject.name.Names
import javax.inject.{Inject, Provider, Singleton}
import kvothe.utility.git.{GitExecutor, GrantsGitExecutor, SheetsGitExecutor}
import monix.execution.Scheduler

import scala.concurrent.ExecutionContext

class KvthModule extends AbstractModule {
  def configure(): Unit = {

    bind(classOf[GitExecutor])
      .annotatedWith(Names.named("sheets"))
      .to(classOf[SheetsGitExecutor])
    bind(classOf[Scheduler])
        .toProvider(classOf[SchedulerProvider])
    bind(classOf[GitExecutor])
      .annotatedWith(Names.named("grants"))
      .to(classOf[GrantsGitExecutor])
  }
}
@Singleton
class SchedulerProvider @Inject()(executionContext: ExecutionContext) extends Provider[Scheduler]{
  private val scheduler = Scheduler(executionContext)
  override def get(): Scheduler = scheduler
}