package kvothe.rost

import kvothe.utility.json.KvotheWriters
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Injecting

import scala.concurrent.duration._
import scala.util.Try

class GrowerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll with KvotheWriters {


  val node4 = Grower.fork[Task, Char, JsValue](_.branch("optProp".seg).option {
    char =>
      Task eval Some(char.value).filter(_ != 'l').map(_.toString)
  }.into(Grower.leaf[Task, String, JsValue]))

  val node3 = Grower.fork[Task, String, JsValue](_.branch("arrProp".seg).seq {
    str =>
      Task eval str.value.toList
  }.into(node4))

  val node2 = Grower.fork[Task, String, JsValue](_.branch("mapProp".seg).map {
    greeting =>
      Task.eval(greeting.value.groupBy(_.toString))
  }.into(node3))

  val node1 = Grower.fork[Task, String, JsValue](_.branch[String]("normalProp".seg).id {
    string => Task eval (string.value + "World")
  }.into(node2))

  "EdgeBuilder" should {
    "work" in {
      implicit val xxx = inject[Scheduler]
      val attempt1: Try[Task[JsValue]] = node1.handle("Hello", Request(Vector(""), 0, Nil, ""))

      attempt1.isFailure mustBe true
      val attempt2: Try[Task[JsValue]] = node1.handle("Hello", Request(Vector(
        "normalProp",
        "mapProp",
        "arrProp",
        "optProp"), 0, Nil, ""))


      attempt2.get.runSyncUnsafe(10.seconds) mustEqual Json.parse(
        """{
          |  "normalProp" : {
          |    "mapProp" : {
          |      "e" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "e"
          |          }
          |        ]
          |      },
          |      "l" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : null
          |          },
          |          {
          |            "optProp" : null
          |          },
          |          {
          |            "optProp" : null
          |          }
          |        ]
          |      },
          |      "H" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "H"
          |          }
          |        ]
          |      },
          |      "W" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "W"
          |          }
          |        ]
          |      },
          |      "r" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "r"
          |          }
          |        ]
          |      },
          |      "o" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "o"
          |          },
          |          {
          |            "optProp" : "o"
          |          }
          |        ]
          |      },
          |      "d" : {
          |        "arrProp" : [
          |          {
          |            "optProp" : "d"
          |          }
          |        ]
          |      }
          |    }
          |  }
          |}
        """.stripMargin)

    }
  }

}
