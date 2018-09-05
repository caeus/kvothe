package utility.git

import java.nio.file.Paths

import scala.concurrent.duration._
import scala.reflect.ClassTag

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.google.inject.ImplementedBy
import com.softwaremill.tagging._
import javax.inject.{Inject, Singleton}
import monix.eval.Task
import org.eclipse.jgit.api.Git
import utility.git.internal.GitTransactor
import utility.git.types.cmds.{GitScript, GitTx}

@ImplementedBy(classOf[GuiceGitActorExecutor])
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
class GuiceGitActorExecutor @Inject()(lazyGitHolder: LazyGitHolder, system: ActorSystem)
  extends GitActorExecutor(transactor = system.actorOf(GitTransactor.props(lazyGitHolder.repo))
    .taggedWith[GitTransactor]
  )(Timeout(10.seconds))

@Singleton
class LazyGitHolder {
  val repo: Git = Git.open(Paths.get("internal/test").toFile)
}