import kvothe.domain.UserId
import kvothe.utility.json.{KvotheReaders, KvotheWriters}
import monix.eval.Task
import monix.execution.Scheduler
import play.api.mvc._

package object controllers extends KvotheWriters with KvotheReaders {


  implicit class AuthSessionOps[A](builder: ActionBuilder[Request, A]) {
    def withSession(block: Request[A] => UserId => Task[Result])
                   (implicit scheduler: Scheduler): Action[A] = {
      builder.async {
        request =>
          request.session.data.get("userId").map(UserId).map(block(request))
            .getOrElse(Task.raiseError(new Exception("WTOAKSDLAKSD")))
            .runAsync(scheduler)
      }
    }

  }

}


