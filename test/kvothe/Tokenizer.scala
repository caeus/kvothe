package kvothe

import scala.annotation.tailrec
import scala.collection.immutable

import fastparse._
import NoWhitespace._
import cats.data.{Ior, NonEmptyVector}
import enumeratum.{Enum, EnumEntry}
import kvothe.FSM.FSMState
import kvothe.FSM.FSMState.{ConcatS, IndexS, IorS}
import kvothe.RExpression._
import kvothe.Tokenizer.Predicate


/*
Ok, so? Why am I even wanting to create a lanbguage?
1. It's fun, and that's enough
2. As a very versatile way of extending kvothe to allow it to have different RPG systems
All rpg systems rely on some random generation, (N)PC sheets and players outside input.
we could create a system in which initiative is the value in certain path plus some random value from 1 to 10.
We could create a brawling shit
But wait, there are two big different things here
1. Updating a character sheet (Json patch?) and
2. Random generators reading character sheets
Ok, so designing a new language or using an existing one
If I want to create a new one I need, specifically
nonJsonExpressions, which are????
1. immutable assignments
let (
 name := expr,
 name2:= expr
) in func(name)(name2)


2. lambda function definition
(x)=>expr

3. function application
(((fun arg0) arg1) arg2) same to down
(fun (arg0 (arg1 arg2)))
fun $ arg0 $ arg1 $ arg2
{fun>arg}
< expr @ expr >
if ternary
 */
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

trait BaseKDefinitions {
  val isSign: Predicate = "+-".toSet

  val isValidSymbol: Predicate = """+-=*&^%$#@!~?<>/\|""".toSet

  val isUnderscore: Predicate = Set('_')

  val isDelimiter: Predicate = """{}(),.[]:;""".toSet

  private val digitsStr = "0123456789"
  val isDigit: Predicate = CharPredicates.isDigit
  private val hexAlphaStr = "abcdef"
  val isHexDigit: Predicate = (digitsStr + hexAlphaStr + hexAlphaStr.toUpperCase).toSet

  val isValidStringChar: Predicate = Set('"', '\\').andThen(!_)

  val isLetter: Predicate = CharPredicates.isLetter
  val isKeyword: String => Boolean = Set.empty
  val isBoolean: String => Boolean = Set("true", "false")
  val isNull: String => Boolean = _ == "null"
}

object Tokenizer extends BaseKDefinitions {
  type Predicate = Char => Boolean
  implicit class PredicateOps(val value: Predicate) extends AnyVal {
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
  def buildTokenizer[_: P](func: KTokenKind => P[String]) = {

    P(Start ~ (f_space ~ KTokenKind.values.map {
      kind =>
        func(kind).map(s => KToken(kind, s))
    }.reduce(_ | _)).rep.map(_.toVector) ~ f_space ~ End)
  }

  import kvothe.KTokenKind._


  def tokenizer2[_: P] = {
    P(Start ~ P("[[").!.rep ~ End)
  }

  def tokenFor[_: P](kind: KTokenKind): P[KToken] = (kind match {
    case OpenParenthesis => LiteralStr("(").!
    case CloseParenthesis => LiteralStr(")").!
    case OpenBracket => LiteralStr("[").!
    case CloseBracket => LiteralStr("]").!
    case OpenCurly => LiteralStr("{").!
    case CloseCurly => LiteralStr("}").!
    case Comma => LiteralStr(",").!
    case Colon => LiteralStr(":").!
    case Semicolon => LiteralStr(";").!
    case Equals => LiteralStr("=").!
    case Arrow => LiteralStr("=>").!
    case Let => LiteralStr("let").!
    case BoolLiteral => (LiteralStr("true") | LiteralStr("false")).!
    case NullLiteral => LiteralStr("null").!
    case Identifier => f_identifierP | f_symbolIdP
    case StringLiteral => f_stringLiteralP
    case NumberLiteral => f_numberLiteralP
  }).map(KToken(kind, _))

  //this sucks, I cannot just joing them all using a fold or something because fastparse uses heavily macros, and SO, it's buggy as fuck
  def singleToken[_: P] = P(tokenFor(OpenParenthesis) |
    tokenFor(CloseParenthesis) |
    tokenFor(OpenBracket) |
    tokenFor(CloseBracket) |
    tokenFor(OpenCurly) |
    tokenFor(CloseCurly) |
    tokenFor(Comma) |
    tokenFor(Colon) |
    tokenFor(Semicolon) |
    tokenFor(Arrow) |
    tokenFor(Equals) |
    tokenFor(Let) |
    tokenFor(BoolLiteral) |
    tokenFor(NullLiteral) |
    tokenFor(Identifier) |
    tokenFor(StringLiteral) |
    tokenFor(NumberLiteral)
  )

  def tokenizer[_: P]: P[Vector[KToken]] = {
    P(Start ~ (f_space ~ singleToken).rep.map(_.toVector) ~ f_space ~ End)
  }

  //All programs are compose of one line


  def f_digits[_: P] = P(CharsWhile(isDigit))

  def f_exponent[_: P] = P(CharIn("eE") ~ CharPred(isSign).? ~ f_digits)

  def f_fractional[_: P] = P("." ~ f_digits)

  def f_integral[_: P] = P("0" | CharPred {
    case '0' => false
    case x => isDigit(x)
  } ~ f_digits.?)

  def f_numberLiteralP[_: P] = P(CharPred(isSign).? ~ f_integral ~ f_fractional.? ~ f_exponent.?).!


  def f_hexDigit[_: P] = P(CharPred(isHexDigit))

  def f_stringUnicodeEscapedChar[_: P] = P("u" ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit ~ f_hexDigit)

  def f_stringEscapedChar[_: P] = P( """\""" ~ (CharPred( """"/\bfnrt""".toSet) | f_stringUnicodeEscapedChar))

  def f_stringChars[_: P] = P(CharsWhile(isValidStringChar))

  def f_stringLiteralP[_: P] = P("\"" ~ (f_stringChars | f_stringEscapedChar).rep.! ~ "\"")

  def f_symbolIdP[_: P] = P(CharsWhile(isValidSymbol)).!

  def f_identifierP[_: P] = P(CharsWhile(isLetter) ~ CharPred(isLetter || isDigit || isUnderscore).rep).!

  def f_space[_: P] = P(CharsWhileIn(" \r\n", 0))
}
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

case class KParser(vector: Vector[KToken]) extends BaseKDefinitions {

  implicit class _KTokenKindAsParser[_: P](value: KTokenKind) {
    def p: P[Int] = P("|" ~ KTokenKind.indexOf(value).toString ~ ":" ~ CharsWhile(isDigit).!.map(
      s => s.toInt))
  }
  import KTokenKind._


  def arrayLiteral[_: P] = P(OpenBracket.p ~ expression_real.rep(sep = Comma.p) ~
    CloseBracket.p).map(vals => RArray(vals._2))

  private def stringLiteral[_: P]: P[RString] = StringLiteral.p.map(vector).map(_.value).map(RString)

  def objectLiteral[_: P] = P(OpenCurly.p ~
    (stringLiteral ~ Colon.p ~ expression_real).rep(sep = Comma.p) ~ CloseCurly.p).map(_._2.map {
    case (key, _, value) => key.value -> value
  }.toMap match {
    case x => RObject(x)
  })

  def enclosedExp[_: P] = P(OpenParenthesis.p ~ expression_real ~
    CloseParenthesis.p).map(_._2)

  private def identifierExpression[_: P]: P[RReference] = P(Identifier.p).map(s=>RReference(vector(s).value))

  def letExpression[_: P] = P(Let.p ~ identifierExpression ~ Equals.p ~
    expression_real ~
    Semicolon.p ~ expression_real).map {
    case (_, bind, _, to, _, in) => RLetExpression(bind, to, in)
  }


  def lambdaExpression[_: P] = P(OpenParenthesis.p ~
    identifierExpression ~ CloseParenthesis.p ~ Arrow.p ~ expression_real).map {
    case (_, arg, b, c, body) => RLambdaExpression(None, arg, body)
  }

  def numberLiteral[_: P] = P(NumberLiteral.p).map { s => RNumber(BigDecimal(vector(s).value)) }

  def functionCallExpression[_: P] = P(expression_real ~
    OpenParenthesis.p ~ expression_real ~ CloseParenthesis.p)


  def nullLiteral[_: P] = P(NullLiteral.p).map(_ => RNull)

  def boolLiteral[_: P] = P(BoolLiteral.p).map(s => RBoolean(vector(s).value.toBoolean))

  def expression_real[_: P]: P[RExpression] = P(
    stringLiteral |
      numberLiteral |
      boolLiteral |
      nullLiteral |
      arrayLiteral |
      objectLiteral |
      lambdaExpression |
      //enclosed under lambda
      enclosedExp |
      letExpression|
    identifierExpression
  )

  def expresssion[_: P]: P[Any] = P(
    functionCallExpression |
      letExpression |
      lambdaExpression |
      stringLiteral |
      NumberLiteral.p.map(vector).map(_.value).map(s => RNumber(BigDecimal(s))) |
      NullLiteral.p.map(_ => RNull) |
      BoolLiteral.p.map(vector).map(_.value).map(RString) |
      identifierExpression)

}

object Splitter {

}


//cool, but for later
trait FSM[In] {


  def initialState: FSMState
  def isFinalState: FSMState => Boolean
  //Option of State because
  def run(in: In, state: FSMState = initialState): Option[FSMState]

  //utility really
  final def contramap[NewIn](in: NewIn => In): FSM[NewIn] = ???

  final def concat(machines: NonEmptyVector[FSM[In]]): FSM[In] = {
    FSM.concat(this, machines)
  }

  final def fork(other: FSM[In]): FSM[In] = {
    FSM.ForkFSM(this, other)
  }
  @tailrec
  private def recursiveDrain(
    input: List[In],
    currentState: FSMState = initialState
  ): Boolean = {
    input match {
      case Nil =>
        isFinalState(currentState)
      case head :: tail =>
        run(head, currentState) match {
          case Some(newState) =>
            recursiveDrain(tail, newState)
          case None => false
        }

    }
  }
  def drain(input: Seq[In]): Boolean = {
    recursiveDrain(input.toList)
  }


}
object FSM {


  def fromSeq[In](matching: Seq[In]): FSM[In] = new FSM[In] {
    override def initialState: FSMState = FSMState.IndexS(0)

    override def isFinalState: FSMState => Boolean = {
      case IndexS(n) if n == matching.length => true
      case _ => false
    }

    override def run(in: In, state: FSMState): Option[FSMState] = {
      state match {
        case IndexS(value) if value == matching.length => None
        case IndexS(value) if value < matching.length =>
          if (matching(value) == in) Some(IndexS(value + 1)) else None
      }
    }
  }

  def concat[In](first: FSM[In], machines: NonEmptyVector[FSM[In]]): FSM[In] = {
    ConcatFSM(first, machines)
  }


  case class ConcatFSM[In](
    first: FSM[In],
    nexts: NonEmptyVector[FSM[In]]
  ) extends FSM[In] {
    private val fsms = nexts.prepend(first)

    override def initialState: FSMState = ConcatS(0, first.initialState)

    override def isFinalState: FSMState => Boolean = {
      case ConcatS(index, state) if index == nexts.length => nexts.last.isFinalState(state)
    }

    override def run(in: In, state: FSMState): Option[FSMState] = {
      state match {
        case ConcatS(n, under) =>
          fsms.get(n).flatMap {
            fsm =>
              fsm.run(in, under).map(ConcatS(n, _): FSMState)
                .orElse {
                  if (fsm.isFinalState(under)) {
                    fsms.get(n + 1).flatMap {
                      nextFsm =>
                        nextFsm.run(in)
                          .map(ConcatS(n + 1, _))
                    }
                  } else {
                    None
                  }
                }
          }
        case _ => throw new Exception("this shouldnt happen")
      }
    }
  }


  case class ForkFSM[In](left: FSM[In], right: FSM[In]) extends FSM[In] {

    override def initialState: FSMState = IorS(Ior.both(left.initialState, right.initialState))

    override def isFinalState: FSMState => Boolean = {
      case IorS(ior) => ior.left.exists(left.isFinalState) || ior.right.exists(right.isFinalState)
      case _ => false
    }

    override def run(in: In, state: FSMState): Option[FSMState] = {
      state match {
        case IorS(ior) =>
          Ior.fromOptions(ior.left.flatMap(s => left.run(in, s)), ior.right.flatMap(s => right.run(in, s)))
            .map(IorS)
        case _ => None
      }
    }
  }

  sealed trait FSMState
  object FSMState {
    case class EitherS(value: Either[FSMState, FSMState]) extends FSMState
    case class ConcatS(value: Int, under: FSMState) extends FSMState
    case class IorS(value: Ior[FSMState, FSMState]) extends FSMState
    case class IndexS(value: Int) extends FSMState
  }

}


sealed trait RExpression


object Fuck {


}