package elodin

import io.plutus.Packer

object ElodinParser {

  import ElodinAST._
  import ElodinTokenKind._
  import io.plutus.Packer.syntax._

  private case class ElodinTokenPredicate(value: ElodinTokenKind) extends Function1[ElodinToken, Boolean] {
    def apply(token: ElodinToken): Boolean = token.kind == value

    override def toString(): String = value.toString
  }
  implicit class _ElodinTokenKindAsParser(value: ElodinTokenKind) {
    def p: Packer[ElodinToken, ElodinToken] = P[ElodinToken](ElodinTokenPredicate(value)).!.map(_.head)
  }

  def arrayLiteral: Packer[ElodinToken, ArrayAST] = P(OpenBracket.p ~> expression.rep(sep = Comma.p) <~
    CloseBracket.p).map(vals => ArrayAST(vals))

  private def stringLiteral: Packer[ElodinToken, StringAST] = StringLiteral.p.map(s => StringAST(s.value))

  def objectLiteral: Packer[ElodinToken, ObjectAST] = P(OpenCurly.p ~>
    (stringLiteral <~> (Colon.p ~> expression)).rep(sep = Comma.p) <~ CloseCurly.p).map(_.map {
    case (key, value) => key.value -> value
  }.toMap match {
    case x => ObjectAST(x)
  })

  def identifierExpression: Packer[ElodinToken, ReferenceAST] = P(Identifier.p).map(s => ReferenceAST(s.value))

  def letExpression: Packer[ElodinToken, LetAST] = P(OpenParenthesis.p ~ Let.p ~>
    (OpenCurly.p ~> (identifierExpression.map(_.value) <~ Colon.p <~> expression).rep(sep = Comma.p) <~ CloseCurly.p) <~>
    expression <~ CloseParenthesis.p).map {
    case (bindings, expression) => LetAST(bindings.toMap, expression)
  }


  def lambdaExpression: Packer[ElodinToken, LambdaAST] = P(OpenParenthesis.p ~ Fn.p ~
    OpenBracket.p ~> identifierExpression.map(_.value).rep(min = 1, sep = Comma.p) <~ CloseBracket.p <~>
    expression <~ CloseParenthesis.p).map {
    case (args, expression) => LambdaAST(args, expression)
  }

  def numberLiteral: Packer[ElodinToken, NumberAST] = P(NumberLiteral.p).map { s => NumberAST(BigDecimal(s.value)) }

  def functionApplyExpression: Packer[ElodinToken, ApplyAST] = P(
    OpenParenthesis.p ~> expression.rep(min = 1) <~ CloseParenthesis.p).map {
    case func :: args =>
      ApplyAST(func, args)
  }

  def nullLiteral: Packer[ElodinToken, NullAST.type] = P(NullLiteral.p).map(_ => NullAST)

  def boolLiteral: Packer[ElodinToken, BooleanAST] = P(BoolLiteral.p).map(s => BooleanAST(s.value.toBoolean))

  def expression = P(
    stringLiteral |
      numberLiteral |
      boolLiteral |
      nullLiteral |
      arrayLiteral |
      objectLiteral |
      lambdaExpression |
      letExpression |
      functionApplyExpression |
      identifierExpression
  )

}