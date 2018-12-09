package trees

import kvothe.api.SheetsApi
import kvothe.rost._
import monix.eval.Task
import play.api.libs.json.JsValue

class SheetsTree {
  def get: Grower[Task, SheetsApi, JsValue] = Grower.fork[Task, SheetsApi, JsValue](
    //_.branch("entries".seg).id(_.value.e)
  )
}
trait Branches{

}