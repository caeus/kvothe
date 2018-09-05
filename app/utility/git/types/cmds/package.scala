
package utility.git.types

import java.nio.file.Path

import akka.util.ByteString
import cats.free.Free
import cats.free.Free.liftF
import utility.akka.TypedQuestion

package object cmds {
  type GitTx[T] = Free[GitOp, T]

  private[git] case class GitScript[T](tx: GitTx[T]) extends TypedQuestion[T]

  sealed trait GitOp[+T]


  object F {
    case class ListFiles(fileName: String) extends GitOp[Seq[Path]]

    def listFiles(fileName: String): Free[GitOp, Seq[Path]] =
      liftF[GitOp, Seq[Path]](ListFiles(fileName))

    case class Touch(fileName: String) extends GitOp[Unit]

    def touch(fileName: String): Free[GitOp, Unit] = liftF[GitOp, Unit](Touch(fileName))

    case class Exists(fileName: String) extends GitOp[Boolean]

    def exists(fileName: String): Free[GitOp, Boolean] = liftF(Exists(fileName))

    case class Write(fileName: String, content: ByteString) extends GitOp[Unit]

    def write(
      fileName: String,
      content: ByteString
    ): Free[GitOp, Unit] = liftF(Write(fileName, content))

    case class Read(fileName: String) extends GitOp[ByteString]

    def read(fileName: String): Free[GitOp, ByteString] = liftF(Read(fileName))
  }

  object G {

    case class Checkout(branchName: String) extends GitOp[Unit]

    def checkout(branchName: String): Free[GitOp, Unit] = liftF(Checkout(branchName))

    case class Commit(message: String,author:String) extends GitOp[CommitRef]

    def commit(comment: String, author:String): Free[GitOp, CommitRef] = liftF(Commit(comment,author))

    case class Add(fileName: String) extends GitOp[Unit]

    def add(fileName: String): Free[GitOp, Unit] = liftF(Add(fileName))

    case class LogOf(fileName: String) extends GitOp[Iterator[CommitRef]]

    def logOf(fileName: String): Free[GitOp, Iterator[CommitRef]] = liftF(LogOf(fileName))

    case class GetCommitRef(id: String) extends GitOp[Option[CommitRef]]

    def getCommitRef(id: String): Free[GitOp, Option[CommitRef]] = liftF(GetCommitRef(id))
  }


  def pure[T](value: T): Free[GitOp, T] = Free.pure(value)


}
