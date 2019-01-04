package kvothe.lang

import scala.collection.immutable

import enumeratum.{Enum, EnumEntry}
import io.plutus.Packer


sealed trait KTokenKind extends EnumEntry

case class KToken(kind: KTokenKind, value: String) {
  override def toString: String = s"$kind:`$value`"
}

object KTokenKind extends Enum[KTokenKind] {
  case object OpenParenthesis extends KTokenKind
  case object CloseParenthesis extends KTokenKind
  case object OpenBracket extends KTokenKind
  case object CloseBracket extends KTokenKind
  case object OpenCurly extends KTokenKind
  case object CloseCurly extends KTokenKind
  case object Comma extends KTokenKind
  case object Colon extends KTokenKind
  case object Semicolon extends KTokenKind
  case object Equals extends KTokenKind
  case object Arrow extends KTokenKind
  case object Let extends KTokenKind
  case object BoolLiteral extends KTokenKind
  case object NullLiteral extends KTokenKind
  case object StringLiteral extends KTokenKind
  case object NumberLiteral extends KTokenKind
  case object Identifier extends KTokenKind
  override def values: immutable.IndexedSeq[KTokenKind] = findValues
}

object KTokenizer {
  import io.plutus.Packer.syntax._
  import kvothe.lang.KTokenKind._

  private type Predicate = Char => Boolean

  private val isValidStringChar: Predicate = Set('"', '\\').andThen(!_)
  private val digitsStr = "0123456789"
  private val hexAlphaStr = "abcdef"
  private val isHexDigit: Predicate = (digitsStr + hexAlphaStr + hexAlphaStr.toUpperCase).toSet
  private val isSign: Predicate = "+-".toSet
  private val isValidSymbol: Predicate = """+-=*&^%$#@!~?<>/\|""".toSet
  private val isUnderscore: Predicate = Set('_')
  private val isDelimiter: Predicate = """{}(),.[]:;""".toSet
  private val isDigit: Predicate = Character.isDigit
  private val isLetter: Predicate = {
    val pattern = "[a-zA-Z_]".r.pattern.asPredicate();
    {
      c: Char => pattern.test(c.toString)
    }
  }


  //  def f_stringLiteralP = P("\"" ~ (f_stringChars | f_stringEscapedChar).rep.! ~ "\"")
  private lazy val f_hexDigit = P(isHexDigit)

  private lazy val f_stringUnicodeEscapedChar: Packer[Char, Unit] = P(P("u") ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit)

  private lazy val f_stringEscapedChar: Packer[Char, Unit] = P("""\""") ~ (P( """"/\bfnrt""".toSet) | f_stringUnicodeEscapedChar)

  private lazy val f_stringChars: Packer[Char, Unit] = P(isValidStringChar)

  private lazy val f_stringLiteralP: Packer[Char, List[Char]] = P("\"") ~> (f_stringChars | f_stringEscapedChar).rep().! <~ P("\"")

  private lazy val f_digit = P(isDigit)

  private lazy val f_space: Packer[Char, List[Unit]] = P(" \r\n".toSet).rep

  private lazy val f_exponent: Packer[Char, Unit] = P[Char](c => "eE".contains(c.toString)) ~ P[Char](isSign).? ~ f_integral

  private lazy val f_fractional: Packer[Char, Unit] = P(".") ~ f_digit.rep(min = 1)

  private lazy val f_integral: Packer[Char, Unit] = (P("0") | P[Char] {
    case '0' => false
    case x => isDigit(x)
  }) ~ f_digit.rep

  private lazy val f_numberLiteralP: Packer[Char, List[Char]] = (P[Char](isSign).? ~ f_integral ~ f_fractional.? ~ f_exponent.?).!

  private lazy val f_symbolIdP: Packer[Char, List[Char]] = P[Char](isValidSymbol).rep(min = 1).!

  private lazy val f_identifierP: Packer[Char, List[Char]] = P(P(isLetter) ~
    P(isLetter || isDigit || isUnderscore).rep).!

  private def buildTokenizer(func: KTokenKind => Packer[Char, List[Char]]): Packer[Char, List[KToken]] = {
    P(f_space ~> KTokenKind.values.map {
      kind =>
        func(kind).map {
          value =>
            KToken(kind, value.mkString)
        } <~ f_space
    }.reduce(_ | _).rep)
  }

  private lazy val tokenizer: Packer[Char, List[KToken]] = buildTokenizer {
    case OpenParenthesis => P("(").!
    case CloseParenthesis => P(")").!
    case OpenBracket => P("[").!
    case CloseBracket => P("]").!
    case OpenCurly => P("{").!
    case CloseCurly => P("}").!
    case Comma => P(",").!
    case Colon => P(":").!
    case Semicolon => P(";").!
    case Equals => P("=").!
    case Arrow => P("=>").!
    case Let => P("let").!
    case BoolLiteral => (P("true") | P("false")).!
    case NullLiteral => P("null").!
    case Identifier => f_identifierP | f_symbolIdP
    case StringLiteral => f_stringLiteralP
    case NumberLiteral => f_numberLiteralP
  }

  def apply(program: String) = tokenizer(program.toList)

}
sealed trait RExpression
object RExpression {
  case class RString(value: String) extends RExpression
  case class RNumber(value: BigDecimal) extends RExpression
  case object RNull extends RExpression
  case class RBoolean(value: Boolean) extends RExpression
  case class RObject(value: Map[String, RExpression]) extends RExpression
  case class RArray(value: Seq[RExpression]) extends RExpression
  case class RReference(value: String) extends RExpression
  case class RLetExpression(
    binding: RReference,
    to: RExpression,
    in: RExpression
  ) extends RExpression
  case class RLambdaExpression(
    self: Option[RReference],
    arg: RReference,
    body: RExpression
  ) extends RExpression
  case class RIfExpression(
    cond: RExpression,
    ifTrue: RExpression,
    ifFalse: RExpression
  ) extends RExpression
  case class RApplyExpression(func: RExpression, arg: RExpression) extends RExpression
}


object KParser {
  import io.plutus.Packer.syntax._
  import kvothe.lang.KTokenKind._
  import kvothe.lang.RExpression._

  implicit class _KTokenKindAsParser(value: KTokenKind) {
    def p: Packer[KToken, KToken] = P[KToken](token => token.kind == value).!.map(_.head)
  }

  def arrayLiteral: Packer[KToken, RArray] = P(OpenBracket.p ~> expression_real.rep(sep = Comma.p) <~
    CloseBracket.p).map(vals => RArray(vals))

  private def stringLiteral: Packer[KToken, RString] = StringLiteral.p.map(s => RString(s.value))

  def objectLiteral: Packer[KToken, RObject] = P(OpenCurly.p ~>
    (stringLiteral <~> (Colon.p ~> expression_real2)).rep(sep = Comma.p) <~ CloseCurly.p).map(_.map {
    case (key, value) => key.value -> value
  }.toMap match {
    case x => RObject(x)
  })

  def enclosedExp: Packer[KToken, RExpression] = P(OpenParenthesis.p ~> expression_real <~
    CloseParenthesis.p)

  private def identifierExpression: Packer[KToken, RReference] = P(Identifier.p).map(s => RReference(s.value))

  def letExpression: Packer[KToken, RLetExpression] = P(Let.p ~> identifierExpression <~ Equals.p <~>
    expression_real <~
    Semicolon.p <~> expression_real2).map {
    case ((bind, to), in) => RLetExpression(bind, to, in)
  }


  def lambdaExpression: Packer[KToken, RLambdaExpression] = P(OpenParenthesis.p ~>
    identifierExpression <~ CloseParenthesis.p <~ Arrow.p <~> expression_real).map {
    case (arg, body) => RLambdaExpression(None, arg, body)
  }

  def numberLiteral: Packer[KToken, RNumber] = P(NumberLiteral.p).map { s => RNumber(BigDecimal(s.value)) }

  def functionCallExpression = P(expression_real ~
    OpenParenthesis.p ~ expression_real ~ CloseParenthesis.p)


  def nullLiteral: Packer[KToken, RNull.type] = P(NullLiteral.p).map(_ => RNull)

  def boolLiteral: Packer[KToken, RBoolean] = P(BoolLiteral.p).map(s => RBoolean(s.value.toBoolean))

  def expression_real = P(
    stringLiteral |
      numberLiteral |
      boolLiteral |
      nullLiteral |
      arrayLiteral |
      objectLiteral |
      lambdaExpression |
      //enclosed under lambda
      enclosedExp |
      letExpression |
      identifierExpression
  )


  def expression_real2 = P(
    stringLiteral |
      numberLiteral |
      boolLiteral |
      nullLiteral |
      arrayLiteral |
      objectLiteral |
      lambdaExpression |
      //enclosed under lambda
      enclosedExp |
      letExpression |
      identifierExpression
  )


}
