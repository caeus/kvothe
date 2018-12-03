package kvothe.util

import cats._
import io.circe.{Encoder, Json, JsonObject}
import kvothe.utility.tson.Tson.{ArrayOf, DictOf, Empty, ValOf}
import kvothe.utility.tson.{TCursor, Tson}
import monix.eval.Task
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.test.Injecting

import scala.concurrent.duration._

class TCursorSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {

  implicit def encoder[A: Encoder]: Encoder[Tson[A]] {
    def apply(tson: Tson[A]): Json
  } = new Encoder[Tson[A]] {
    override def apply(tson: Tson[A]): Json = tson match {
      case ValOf(av) => Encoder[A].apply(av)
      case Empty => Json.Null
      case DictOf(map) => Json.fromJsonObject(JsonObject.fromMap(map.mapValues(apply)))
      case ArrayOf(seq) => Json.fromValues(seq.map(apply))
    }
  }

  "TCursor" should {

    "just work with Idmonad" in {
      val cursor = TCursor.pure[Id]("Hello")
        .down("normalProp")(_ + "World")
        .downMap("mapProp") { asd =>
          asd.groupBy(_.toString)
        }
        .downArr("arrProp") { str =>
          str.toList
        }
        .downOpt("optProp") { char =>
          Some(char).filter(_ != 'l')
        }
        .fold
      println(Encoder[Tson[Char]].apply(cursor))
    }
    "just work with TaskMonad" in {
      import monix.execution.Scheduler.Implicits.global
      val cursor: Tson[Char] = TCursor.pure[Task]("Hello")
        .down("normalProp") { word =>
          Task.eval(word + "World")
        }
        .downMap("mapProp") { greeting =>
          Task.eval(greeting.groupBy(_.toString))
        }
        .downArr("arrProp") { str =>
          Task eval str.toList
        }
        .downOpt("optProp") { char =>
          Task eval Some(char).filter(_ != 'l')
        }
        .fold.runSyncUnsafe(10.seconds)

      println(Encoder[Tson[Char]].apply(cursor))
    }
  }

}
