package kvothe

import io.plutus.Packer
import io.plutus.PackerResult.Done
import kvothe.lang.{KParser, KTokenizer}
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.test.Injecting

class TokenizerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {

  "lakjdlkasjdlk" should {
    "aljksd" in {
      val asd = KTokenizer


      println(KTokenizer(
        """
          |let pepe = leto;
          |{"asd":234.5}
          |
        """.stripMargin.stripMargin) match {
        case Done(value, _) => value
      })
      println(KParser.expression_real2(KTokenizer(
        """
let pepe = leto;
{"asd":234.5}

        """.stripMargin) match {
        case Done(value, _) =>
          println("asjdh"->value)
          value
      }))

      //      println(P("()34").take("()34".toList))
      //      println((P("()34".toSet)~P(")")).take("()34".toList))
      //      println(P("()34".toSet).rep.take("()34".toList))
      //      println((P("()34".toSet)|P(")")).!.take("()34".toList))
      //      println((P("5678".toSet)|P(")")).!.take(")34".toList))
      //
      //      // more helpers!
      //      // string helpers? takeOne, takeWhile
      //      // order
      //
      //
      //
      //      //println(Character.isLetter('1'))
      //      //println(asd.f_identifierP.take(List('1')))
      //        println (asd.tokenizer.take("""
      //                                      |let pepe =leto;
      //                                      |(["ww",4,null,true,{"asd":234.5,"func":(aa)=>5},(pepe)])
      //                                      |
      //        """.stripMargin.toList))
      true mustEqual false
    }

    "a;slkda;lsk" ignore {

    }
  }
}
