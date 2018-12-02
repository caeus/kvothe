package kvothe.utility.git

import java.nio.file.Paths

import scala.concurrent.duration._
import scala.reflect.ClassTag

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.google.inject.ImplementedBy
import com.softwaremill.tagging._
import config.RepoRoots
import javax.inject.{Inject, Named, Singleton}
import monix.eval.Task
import org.eclipse.jgit.api.Git
import kvothe.utility.git.internal.GitTransactor
import kvothe.utility.git.types.cmds.{GitScript, GitTx}


trait GitExecutor {
  def exec[T: ClassTag](script: GitTx[T]): Task[T]
}

class GitActorExecutor(transactor: ActorRef @@ GitTransactor)(implicit timeout: Timeout)
  extends GitExecutor {
  def exec[T: ClassTag](script: GitTx[T]): Task[T] = {
    Task.deferFuture {
      GitScript(script).askTo(transactor)
    }
  }
}
@Singleton
@Named("sheets")
class SheetsGitExecutor @Inject()(repoRoots: RepoRoots, system: ActorSystem)
  extends GitActorExecutor(transactor = system.actorOf(GitTransactor.props(repoRoots.sheets))
    .taggedWith[GitTransactor]
  )(Timeout(10.seconds))

@Singleton
@Named("grants")
class GrantsGitExecutor @Inject()(repoRoots: RepoRoots, system: ActorSystem)
  extends GitActorExecutor(transactor = system.actorOf(GitTransactor.props(repoRoots.grants))
    .taggedWith[GitTransactor]
  )(Timeout(10.seconds))

@Singleton
class LazyGitHolder {
  val repo: Git = Git.open(Paths.get("internal/test").toFile)
}