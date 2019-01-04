package elodin

import elodin.ElodinTokenKind._
import io.plutus.{Packer, PackerResult}

object ElodinLexer {
  import io.plutus.Packer.syntax._
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

  private def buildTokenizer(func: ElodinTokenKind => Packer[Char, List[Char]]): Packer[Char, List[ElodinToken]] = {
    P(f_space ~> ElodinTokenKind.values.map {
      kind =>
        func(kind).map {
          value =>
            ElodinToken(kind, value.mkString)
        } <~ f_space
    }.reduce(_ | _).rep)
  }

  private lazy val tokenizer: Packer[Char, List[ElodinToken]] = buildTokenizer {
    case OpenParenthesis => P("(").!
    case CloseParenthesis => P(")").!
    case OpenBracket => P("[").!
    case CloseBracket => P("]").!
    case OpenCurly => P("{").!
    case CloseCurly => P("}").!
    case Comma => P(",").!
    case Colon => P(":").!
    case Let => P("let").!
    case Fn => P("fn").!
    case BoolLiteral => (P("true") | P("false")).!
    case NullLiteral => P("null").!
    case Identifier => f_identifierP | f_symbolIdP
    case StringLiteral => f_stringLiteralP
    case NumberLiteral => f_numberLiteralP
  }

  def apply(program: String): PackerResult[Char, List[ElodinToken]] = tokenizer(program.toList)

}