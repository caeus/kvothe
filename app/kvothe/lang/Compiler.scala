package kvothe.lang

import scala.collection.immutable

import enumeratum.{Enum, EnumEntry}
import fastparse.CharPredicates
import io.plutus.Packer
import kvothe.lang.KTokenKind._


sealed trait KTokenKind extends EnumEntry

case class KToken(kind: KTokenKind, value: String)

object KTokenKind extends Enum[KTokenKind] {
  //0
  case object OpenParenthesis extends KTokenKind
  //1
  case object CloseParenthesis extends KTokenKind
  //2
  case object OpenBracket extends KTokenKind
  //3
  case object CloseBracket extends KTokenKind
  case object OpenCurly extends KTokenKind
  case object CloseCurly extends KTokenKind
  case object Comma extends KTokenKind
  case object Colon extends KTokenKind
  case object Semicolon extends KTokenKind
  case object Equals extends KTokenKind
  case object Arrow extends KTokenKind
  case object Let extends KTokenKind
  case object Identifier extends KTokenKind
  case object BoolLiteral extends KTokenKind
  case object NullLiteral extends KTokenKind
  case object StringLiteral extends KTokenKind
  case object NumberLiteral extends KTokenKind
  override def values: immutable.IndexedSeq[KTokenKind] = findValues
}

class Compiler {
  private def P(string: String): Packer[Char, Unit] = Packer.fromList(string.toList)

  private def takeWhile(predicate: Predicate): Packer[Char, Unit] = ???

  private def takeOne(predicate: Predicate): Packer[Char, Unit] = ???

  type Predicate = Char => Boolean

  val isValidStringChar: Predicate = Set('"', '\\').andThen(!_)
  private val digitsStr = "0123456789"

  private val hexAlphaStr = "abcdef"
  val isHexDigit: Predicate = (digitsStr + hexAlphaStr + hexAlphaStr.toUpperCase).toSet
  val isSign: Predicate = "+-".toSet

  val isValidSymbol: Predicate = """+-=*&^%$#@!~?<>/\|""".toSet

  val isUnderscore: Predicate = Set('_')

  val isDelimiter: Predicate = """{}(),.[]:;""".toSet

  val isDigit: Predicate = CharPredicates.isDigit


  val isLetter: Predicate = CharPredicates.isLetter
  val isKeyword: String => Boolean = Set.empty
  val isBoolean: String => Boolean = Set("true", "false")
  val isNull: String => Boolean = _ == "null"

  implicit class PredicateOps(value: Predicate) {
    def &&(other: Predicate): Predicate = { char =>
      value(char) && other(char)
    }

    def ||(other: Predicate): Predicate = { char =>
      value(char) || other(char)
    }

    def unary_! : Predicate = { char =>
      !value(char)
    }
  }


  import io.plutus.Packer.syntax._

  //  def f_stringLiteralP[_: P] = P("\"" ~ (f_stringChars | f_stringEscapedChar).rep.! ~ "\"")
  def f_hexDigit = takeOne(isHexDigit)


  def f_stringUnicodeEscapedChar = P("u") ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit

  def f_stringEscapedChar: Packer[Char, Unit] = P("""\""") ~ (takeOne( """"/\bfnrt""".toSet) | f_stringUnicodeEscapedChar)

  def f_stringChars: Packer[Char, Unit] = takeWhile(isValidStringChar)

  def f_stringLiteralP = (P("\"") ~> (f_stringChars | f_stringEscapedChar).rep() <~ P("\"")).map(_.mkString)

  def f_digits = takeWhile(isDigit)

  def f_exponent = takeOne(c => "eE".contains(c.toString)) ~ takeOne(isSign).? ~ f_digits

  def f_fractional = P(".") ~ f_digits

  def f_integral = P("0") | takeOne {
    case '0' => false
    case x => isDigit(x)
  } ~ f_digits.?

  def f_numberLiteralP = (takeOne(isSign).? ~ f_integral ~ f_fractional.? ~ f_exponent.?).!.map(_.mkString)

  def f_symbolIdP = takeWhile(isValidSymbol).!.map(_.mkString)

  def f_identifierP = (takeWhile(isLetter) ~ takeOne(isLetter || isDigit || isUnderscore).rep()).!.map(_.mkString)

  def buildTokenizer(func: KTokenKind => Packer[Char, String]): Packer[List[Char], KToken] = {
    KTokenKind.values.map {
      kind =>
        func(kind).map {
          value =>
            KToken(kind, value)
        }
    }.reduce(_ | _)
  }

  def tokenizer = buildTokenizer {
    case OpenParenthesis => P("(").!.map(_.mkString)
    case CloseParenthesis => P(")").!.map(_.mkString)
    case OpenBracket => P("[").!.map(_.mkString)
    case CloseBracket => P("]").!.map(_.mkString)
    case OpenCurly => P("{").!.map(_.mkString)
    case CloseCurly => P("}").!.map(_.mkString)
    case Comma => P(",").!.map(_.mkString)
    case Colon => P(":").!.map(_.mkString)
    case Semicolon => P(";").!.map(_.mkString)
    case Equals => P("=").!.map(_.mkString)
    case Arrow => P("=>").!.map(_.mkString)
    case Let => P("let").!.map(_.mkString)
    case BoolLiteral => (P("true") | P("false")).!.map(_.mkString)
    case NullLiteral => P("null").!.map(_.mkString)
    case Identifier => f_identifierP | f_symbolIdP
    case StringLiteral => f_stringLiteralP
    case NumberLiteral => f_numberLiteralP
  }


}
