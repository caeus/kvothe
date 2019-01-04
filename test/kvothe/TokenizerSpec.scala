package kvothe

import elodin.{ElodinAST, ElodinLexer, ElodinParser, ElodinToken}
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.test.Injecting

class TokenizerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with BeforeAndAfterAll {

  implicit class StringTokens(value: String) {
    def tokens: List[ElodinToken] = ElodinLexer(value).value
  }
  implicit class RExOps(value: ElodinAST) {
    def print = println(value)
  }
  "Packer" should {
    "just work" in {
      ElodinParser.nullLiteral.take("null   ".tokens).value.print
      ElodinParser.boolLiteral.take("true   ".tokens).value.print
      ElodinParser.boolLiteral.take("   false   ".tokens).value.print
      ElodinParser.numberLiteral.take(" 123.232".tokens).value.print
      ElodinParser.numberLiteral.take(" +123232".tokens).value.print
      ElodinParser.identifierExpression.take("===".tokens).value.print
      ElodinParser.identifierExpression.take("asg".tokens).value.print
      ElodinParser.objectLiteral.take("""{"asd":"asd","qwe":123,"were":false}""".tokens).value.print
      ElodinParser.arrayLiteral.take("[qwe]".tokens).value.print
      ElodinParser.letExpression.take("(let {x:5, y:\"jojo\"} 5)".tokens).value.print
      ElodinParser.lambdaExpression.take("(fn [x,y,u] 5)".tokens).value.print
      ElodinParser.functionApplyExpression.take("(x [x,y,u] 5)".tokens).value.print
      val ast = ElodinParser.expression.take(
        """
          |(
          |let{
          | first:(read ["skills",0]),
          | name:(read ["name"])
          | }
          | (write ["surname"] (concat first name))
          |)
        """.stripMargin.tokens).value.print
      true mustEqual false
    }
  }
}
