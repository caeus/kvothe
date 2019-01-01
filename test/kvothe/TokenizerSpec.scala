package kvothe

import kvothe.rossetta.Tarser
import kvothe.rossetta.Tarser.syntax._
import monix.eval.Task.FlatMap
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.test.Injecting

class TokenizerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {


  "lakjdlkasjdlk" should {
    "aljksd" in {
      val ainfinite = Tarser.fromList("aa".toList).rep()
      println(ainfinite.take("aaaa".toList))

      true mustEqual false
    }

    "a;slkda;lsk" ignore {
      val value = fastparse.parse(
        """
          |let pepe =leto;
          |(["ww",4,null,true,{"asd":234.5,"func":(aa)=>5},(pepe)])
          |
        """.stripMargin, Tokenizer.tokenizer(_)).get.value
      println(value)
      val str = value.zipWithIndex.map {
        case (token, index) =>
          "|" + KTokenKind.indexOf(token.kind) + ":" + index
      }.mkString("")
      println(str)
      val sd = fastparse.parse(str, KParser(value).expression_real(_))

      println(sd)
      //
      //      println(FSM.fromSeq("Camilo".toSeq).concat(NonEmptyVector.one(FSM.fromSeq(" Alejandro".toSeq))).drain("Camilo Alejandro".toSeq))
      //      println(FSM.fromSeq("".toSeq).concat(NonEmptyVector.one(FSM.fromSeq(" Alejandro".toSeq))).drain(" Alejandro".toSeq))
      //      println(FSM.fromSeq("".toSeq).drain(" Alejandro".toSeq))
      //      println(FSM.fromSeq("".toSeq).drain("".toSeq))
      //      println(FSM.fromSeq("Camilo".toSeq).concat(NonEmptyVector.one(FSM.fromSeq(" Alejandro".toSeq))).drain("Camilo Alejandro x ".toSeq))
      //      println(FSM.fromSeq("Camilo".toSeq).concat(NonEmptyVector.one(FSM.fromSeq(" Alejandro".toSeq))).drain("Camilo Alejandr".toSeq))
      //      println(FSM.fromSeq("Camilo".toSeq).fork(FSM.fromSeq("Alejandro".toSeq)).drain("Camilo".toSeq) -> "check this one, must be true")
      //      println(FSM.fromSeq("Camilo".toSeq).fork(FSM.fromSeq("Alejandro".toSeq)).drain("Alejandro".toSeq) -> "check this one, must be true")
      //      println(FSM.fromSeq("Camilo".toSeq).fork(FSM.fromSeq("Alejandro".toSeq)).drain("qwioe".toSeq) -> "check this one, must be false")
      //      println(FSM.fromSeq("Camilo".toSeq).drain("Camilo".toSeq))
      //      println(FSM.fromSeq("Camilo".toSeq).drain("Camiloo".toSeq))
      //      println(FSM.fromSeq("Camilo".toSeq).drain("Camiloo".toSeq))
      //
      //      "a*".r.findAllMatchIn("aaaab").foreach(x => println(x.start))
      //      println("a*".r.findAllMatchIn("aaaab").length)

      //First state! Should there be a state for when the input is not matched? not expected?
      true mustEqual true
    }
  }
}
