package kvothe.sower

import edu.caeus.herbivicus.sower.ConcreteSower
import edu.caeus.herbivicus.sower.dsl.{SowerDSL, from}
import edu.caeus.herbivicus.vine.Vine.PartialFolder
import monix.eval.Task
import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue}

class KvotheSower extends ConcreteSower[Task, JsValue](PartialFolder[JsValue](JsNull,
  array => JsArray(array),
  dict => JsObject(dict))) {

  val dsl: SowerDSL[Task, JsValue] = from(this)
}
object KvotheSower extends KvotheSower