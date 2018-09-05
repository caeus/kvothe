package utility.monix

import monix.eval.Task

trait TaskImplicits {

  implicit def enrichTask2TaskOps[T](task: Task[T]):TaskOps[T]=new TaskOps[T](task)

}

class TaskOps[T](val value: Task[T]) extends AnyVal {
  def otherwise[TT](error: => Throwable)(implicit ev: T <:< Option[TT]): Task[TT] = {
    value.flatMap { inner =>
      ev(inner).map(Task.pure).getOrElse(Task.raiseError(error))
    }
  }
}
