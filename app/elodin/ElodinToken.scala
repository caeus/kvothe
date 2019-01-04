package elodin

import scala.collection.immutable

import enumeratum.{Enum, EnumEntry}

sealed trait ElodinTokenKind extends EnumEntry

case class ElodinToken(kind: ElodinTokenKind, value: String) {
  override def toString: String = s"$kind:`$value`"
}

object ElodinTokenKind extends Enum[ElodinTokenKind] {
  case object OpenParenthesis extends ElodinTokenKind
  case object CloseParenthesis extends ElodinTokenKind
  case object OpenBracket extends ElodinTokenKind
  case object CloseBracket extends ElodinTokenKind
  case object OpenCurly extends ElodinTokenKind
  case object CloseCurly extends ElodinTokenKind
  case object Comma extends ElodinTokenKind
  case object Colon extends ElodinTokenKind
  case object Let extends ElodinTokenKind
  case object Fn extends ElodinTokenKind
  case object BoolLiteral extends ElodinTokenKind
  case object NullLiteral extends ElodinTokenKind
  case object StringLiteral extends ElodinTokenKind
  case object NumberLiteral extends ElodinTokenKind
  case object Identifier extends ElodinTokenKind
  override def values: immutable.IndexedSeq[ElodinTokenKind] = findValues
}