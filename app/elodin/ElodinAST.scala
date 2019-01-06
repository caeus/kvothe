package elodin

import elodin.ElodinAST.EloVisitor

sealed trait ElodinAST {
  def accept[R](visitor:EloVisitor[R]):R
}
object ElodinAST {

  trait EloVisitor[R] {
    def visitString(value: String): R
    def visitNumber(value: BigDecimal): R
    def visitNull: R
    def visitBoolean(value: Boolean): R
    def visitObject(fields: Map[String, ElodinAST]): R
    def visitArray(items: Seq[ElodinAST]): R
    def visitRef(ref: String): R
    def visitLet(bindings: Map[String, ElodinAST], body: ElodinAST): R
    def visitLambda(args: Seq[String], body: ElodinAST): R
    def visitApply(func: ElodinAST, args: List[ElodinAST]): R
  }

  case class StringAST(value: String) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitString(value)
  }
  case class NumberAST(value: BigDecimal) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitNumber(value)
  }
  case object NullAST extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitNull
  }
  case class BooleanAST(value: Boolean) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitBoolean(value)
  }
  case class ObjectAST(value: Map[String, ElodinAST]) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitObject(value)
  }
  case class ArrayAST(value: Seq[ElodinAST]) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitArray(value)
  }
  case class ReferenceAST(value: String) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitRef(value)
  }
  case class LetAST(
    binding: Map[String, ElodinAST],
    in: ElodinAST
  ) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitLet(binding,in)
  }
  case class LambdaAST(
    args: Seq[String],
    body: ElodinAST
  ) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitLambda(args,body)
  }
  case class ApplyAST(
    func: ElodinAST,
    args: List[ElodinAST]
  ) extends ElodinAST {
    override def accept[R](visitor: EloVisitor[R]): R = visitor.visitApply(func,args)
  }
}