package kvothe.sower

import io.sower.{ConcreteSower, SowerDSL}
import io.vine.Vine.Combiner
import monix.eval.Task
import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue}

case class KvotheReq(query: Map[String, Seq[String]], body: JsValue)
class KvotheSower extends ConcreteSower[Task, KvotheReq, JsValue](Combiner[JsValue](JsNull,
  JsArray(_),
  JsObject(_))) with SowerDSL[Task, KvotheReq, JsValue]
object KvotheSower extends KvotheSower