package kvothe.utility.git.internal

import java.nio.file._

import scala.util.{Failure, Success, Try}

import akka.actor.{Actor, Props, Status}
import akka.util.ByteString
import cats.implicits._
import cats.~>
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.errors.MissingObjectException
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.RevWalk
import kvothe.utility.git.types.cmds.{GitScript, _}
import kvothe.utility.git.types.{CommitRef, GitBackedCommit}

class GitTransactor(repo: Git) extends Actor {

  val interpreter = new GitScriptInterpreter(repo)

  override def receive: Receive = {
    case GitScript(tx) =>
      Try {
        tx.foldMap(interpreter)
      }.flatten match {
        case Success(value) => sender() ! value
        case Failure(e) => sender() ! Status.Failure(e)
      }

  }
}

object GitTransactor {
  def props(repo: Git) = Props(new GitTransactor(repo))
}

class GitScriptInterpreter(git: Git) extends (GitOp ~> Try) {
  import scala.collection.JavaConverters._

  val rootDir: Path = git.getRepository.getWorkTree.toPath

  override def apply[A](fa: GitOp[A]): Try[A] = {

    fa match {
      case F.Exists(fileName) => Try {
        Files.exists(rootDir.resolve(fileName))
      }
      case F.Touch(fileName) => Try {
        val path = rootDir.resolve(fileName)
        Option(path.getParent).foreach {
          parent => Files.createDirectories(parent)
        }
        Files.createFile(path)
        ()
      }
      case F.Read(fileName) => Try {
        ByteString(Files.readAllBytes(rootDir.resolve(fileName)))
      }
      case F.Write(fileName, content) => Try {
        val out = Files.newByteChannel(rootDir.resolve(fileName), StandardOpenOption.WRITE)
        out.write(content.toByteBuffer)
        out.close()
        ()
      }
      case F.ListFiles(fileName) => Try {
        Files.list(rootDir.resolve(fileName)).iterator().asScala.toSeq
      }
      case G.Checkout(branchName) => Try {
        git.checkout().setName(branchName).call()
        ()
      }
      case G.Commit(comment, author) => Try {
        new GitBackedCommit(git.commit().setMessage(comment).setAuthor(author, s"$author@kvothe").call())
      }
      case G.Add(fileName) => Try {
        git.add().addFilepattern(fileName).call()
        ()
      }
      case G.LogOf(fileName) => Try {
        import scala.collection.JavaConverters._
        git.log().addPath(fileName).call().iterator().asScala.map {
          commitRev =>
            new GitBackedCommit(commitRev)
        }
      }
      case G.GetCommitRef(id) => Try {
        val walk = new RevWalk(git.getRepository)
        walk.close()
        new GitBackedCommit(walk.parseCommit(ObjectId.fromString(id)))
      } match {
        case Success(v: CommitRef) => Success(Some(v))
        case Failure(e: MissingObjectException) => Success(None)
        case Failure(e) => Failure(e)
      }
    }

  }
}



