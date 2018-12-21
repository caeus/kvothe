package kvothe.api

import java.time.Instant

import scala.util.Try

import monix.eval.Task


case class ThreadRef(id: String)
case class ThreadDetails(id: String)
case class ThreadComment(
  id: String,
  data: String,
  when: Instant,
  author: String
)
case class StartThreadRequest(participants: Set[String])
case class StartThreadResponse(participants: Set[String])
case class PostCommentRequest(data: String)
case class PostCommentResponse(data: String)

trait ThreadApi {
  def comments(offset: Long, limit: Long): Task[Seq[ThreadComment]]
  def comment(postCommentRequest: PostCommentRequest): Task[Try[PostCommentResponse]]
  def details: Task[ThreadDetails]
}

class DefaultThreadApi(ref:ThreadRef) extends ThreadApi {
  override def comments(
    offset: Long,
    limit: Long
  ): Task[Seq[ThreadComment]] = ???

  override def comment(postCommentRequest: PostCommentRequest): Task[Try[PostCommentResponse]] = ???

  override def details: Task[ThreadDetails] = ???
}
trait ForumApi {
  def threads(query: String, offset: Long, limit: Long): Task[Seq[ThreadRef]]
  def thread(id: String): Task[Option[ThreadApi]]
  def start(startThreadRequest: StartThreadRequest): Task[Try[StartThreadResponse]]
}

class DefaultForumApi{

}
