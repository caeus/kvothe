package kvothe.depos

import java.nio.file.{Files, Path, SimpleFileVisitor}

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevCommit

object RestartRepo {

  def apply(at:Path): RevCommit ={
    Files.walkFileTree(at, new SimpleFileVisitor[Path] {

      import java.io.IOException
      import java.nio.file.attribute.BasicFileAttributes
      import java.nio.file.{FileVisitResult, Files}

      @throws[IOException]
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      @throws[IOException]
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
    Files.createDirectory(at)

    val repo = Git.init().setDirectory(at.toFile).call()
    repo.commit().setAll(true).setMessage("Initial commit for test").call()
  }

}
