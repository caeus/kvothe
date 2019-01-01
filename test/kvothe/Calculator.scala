package kvothe

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait WorkflowToken

case class IDENTIFIER(str: String) extends WorkflowToken
case class LITERAL(str: String) extends WorkflowToken
case class INDENTATION(spaces: Int) extends WorkflowToken
case object EXIT extends WorkflowToken
case object READINPUT extends WorkflowToken
case object CALLSERVICE extends WorkflowToken
case object SWITCH extends WorkflowToken
case object OTHERWISE extends WorkflowToken
case object COLON extends WorkflowToken
case object ARROW extends WorkflowToken
case object EQUALS extends WorkflowToken
case object COMMA extends WorkflowToken
case object INDENT extends WorkflowToken
case object DEDENT extends WorkflowToken
trait WorkflowCompilationError
case class WorkflowLexerError(msg: String) extends WorkflowCompilationError
object Calculator extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r
  def identifier: Parser[IDENTIFIER] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }
  def literal: Parser[LITERAL] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }
  def exit          = "exit"          ^^ (_ => EXIT)
  def readInput     = "read input"    ^^ (_ => READINPUT)
  def callService   = "call service"  ^^ (_ => CALLSERVICE)
  def switch        = "switch"        ^^ (_ => SWITCH)
  def otherwise     = "otherwise"     ^^ (_ => OTHERWISE)
  def colon         = ":"             ^^ (_ => COLON)
  def arrow         = "->"            ^^ (_ => ARROW)
  def equals        = "=="            ^^ (_ => EQUALS)
  def comma         = ","             ^^ (_ => COMMA)

  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(exit | readInput | callService | switch | otherwise | colon | arrow
      | equals | comma | literal | identifier | indentation)) ^^ { rawTokens =>
      rawTokens
    }
  }

  def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}