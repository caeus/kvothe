package kvothe.util

import cats._
import io.circe.{Encoder, Json, JsonObject}
import kvothe.utility.vine.{VineT, Vine}
import monix.eval.Task
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.test.Injecting

import scala.concurrent.duration._

class VineTSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {

  implicit def encoder[A: Encoder]: Encoder[Vine[A]] {
    def apply(tson: Vine[A]): Json
  } = new Encoder[Vine[A]] {
    override def apply(tson: Vine[A]): Json = tson.fold(
      Json.Null,
      av => Encoder[A].apply(av),
      seq => Json.fromValues(seq.map(apply)),
      map => Json.fromJsonObject(JsonObject.fromMap(map.mapValues(apply)))
    )
  }

  "TCursor" should {

    "just work with Idmonad" in {
      val cursor = VineT.pure[Id]("Hello")
        .growVal("normalProp")(_ + "World")
        .growMap("mapProp") { asd =>
          asd.groupBy(_.toString)
        }
        .growSeq("arrProp") { str =>
          str.toList
        }
        .growOpt("optProp") { char =>
          Some(char).filter(_ != 'l')
        }
        .collapse
      //println(Encoder[Tson[Char]].apply(cursor))
    }
    "just work with TaskMonad" in {
      import monix.execution.Scheduler.Implicits.global
      val cursor: Vine[Char] = VineT.pure[Task]("Hello")
        .growVal("normalProp") { word =>
          Task.eval(word + "World")
        }
        .growMap("mapProp") { greeting =>
          Task.eval(greeting.groupBy(_.toString))
        }
        .growSeq("arrProp") { str =>
          Task eval str.toList
        }
        .growOpt("optProp") { char =>
          Task eval Some(char).filter(_ != 'l')
        }
        .collapse.runSyncUnsafe(10.seconds)

      println(Encoder[Vine[Char]].apply(cursor))
    }
  }

}
