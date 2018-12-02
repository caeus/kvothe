package kvothe.utility.git

import org.eclipse.jgit.revwalk.RevCommit

package object types {




  sealed trait CommitRef {
    def id: String

    def author:String

    def message:String
  }

  private[git] class GitBackedCommit(revCommit: RevCommit) extends CommitRef {
    override lazy val id = revCommit.getId.name()

    override lazy val author: String = revCommit.getAuthorIdent.getName

    override def message: String = revCommit.getFullMessage
  }


}
