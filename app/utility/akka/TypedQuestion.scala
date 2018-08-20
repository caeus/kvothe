package utility.akka

import scala.concurrent.Future
import scala.reflect.ClassTag

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

trait TypedQuestion[T] {

  def askTo(actorRef:ActorRef)(implicit timeout: Timeout, classTag: ClassTag[T]): Future[T] ={
    (ask(actorRef) ? this).mapTo[T]
  }
}
