package utility.git.internal

import java.io.File
import java.nio.file._

import scala.io.Source
import scala.util.{Failure, Success, Try}

import akka.actor.{Actor, Props, Status}
import akka.util.ByteString
import cats.implicits._
import cats.~>
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.errors.MissingObjectException
import org.eclipse.jgit.lib.{ObjectId, PersonIdent}
import org.eclipse.jgit.revwalk.RevWalk
import utility.git.types.cmds.{GitScript, _}
import utility.git.types.{CommitRef, GitBackedCommit}

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
  override def apply[A](fa: GitOp[A]): Try[A] = {

    fa match {
      case F.Exists(fileName) => Try {
        new File(git.getRepository.getWorkTree, fileName).exists()
      }
      case F.Touch(fileName) => Try {
        new File(git.getRepository.getWorkTree, fileName).createNewFile()
        ()
      }
      case F.Read(fileName) => Try {
        ByteString(Source.fromFile(new File(git.getRepository.getWorkTree, fileName)).mkString)
      }
      case F.Write(fileName, content) => Try {
        val out = Files.newByteChannel(new File(git.getRepository.getWorkTree, fileName).toPath, StandardOpenOption.WRITE)
        out.write(content.toByteBuffer)
        out.close()
        ()
      }
      case G.Checkout(branchName) => Try {
        git.checkout().setName(branchName).call()
        ()
      }
      case G.Commit(comment,author) => Try {
        new GitBackedCommit(git.commit().setMessage(comment).setAuthor(author,s"$author@kvothe").call())
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
        case Success(v:CommitRef)=> Success(Some(v))
        case Failure(e:MissingObjectException)=> Success(None)
        case Failure(e) => Failure(e)
      }
    }

  }
}



