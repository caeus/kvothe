import scala.concurrent.ExecutionContext

import model.UserId
import monix.eval.Task
import monix.execution.Scheduler
import play.api.mvc._
import gnieh.diffson.playJson._

package object controllers {


  implicit def schedulerFromExecutionContext(implicit ec:ExecutionContext): Scheduler ={
    Scheduler(ec)
  }

  implicit class AuthSessionOps[A](builder:ActionBuilder[Request,A]){
    def withSession(block:Request[A]=>UserId=>Task[Result])
      (implicit scheduler: Scheduler): Action[A] ={
      builder.async{
        request=>
          request.session.data.get("userId").map(UserId).map(block(request))
              .getOrElse(Task.raiseError(new Exception("WTOAKSDLAKSD")))
              .runAsync(scheduler)
      }
    }

  }

}


