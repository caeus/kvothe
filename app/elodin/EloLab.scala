package elodin

import cats.Eval
import elodin.ElodinAST.EloVisitor

object EloLab {

  sealed trait EloVal
  class EloNumber(val value:BigDecimal) extends EloVal
  class EloGeneric[R](val value:R) extends EloVal
  class EloNull() extends EloVal
  class EloArray(values:Seq[EloVal]) extends EloVal
  class EloObject(fields:Map[String,EloVal]) extends EloVal
  class EloFunction() extends EloVal{
    def apply(args:EloVal*)= ???
  }

  sealed trait EloScope {
    def get(name: String): Option[Any]
  }
  object EloRootScope extends EloScope {
    override def get(name: String): Option[Any] = None
  }
  case class ChildEloScope(
    bindings: Map[String, Any],
    parent: EloScope
  ) extends EloScope {
    override def get(name: String): Option[Any] = bindings.get(name).orElse(parent.get(name))
  }

  def run(ast: ElodinAST): Eval[Any] = {
    ast.accept(new EloVisitor[EloVal] { visitor=>
      override def visitString(value: String): EloVal = new EloGeneric(value)

      override def visitNumber(value: BigDecimal): EloVal = new EloGeneric(value)

      override def visitNull: EloVal = new EloNull

      override def visitBoolean(value: Boolean): EloVal = new EloGeneric(value)

      override def visitObject(fields: Map[String, ElodinAST]): EloVal = ???

      override def visitArray(items: Seq[ElodinAST]): EloVal = new EloArray(items.map(_.accept(visitor)))

      override def visitRef(ref: String): EloVal = ???

      override def visitLet(
        bindings: Map[String, ElodinAST],
        body: ElodinAST
      ): EloVal = ???

      override def visitLambda(
        args: Set[String],
        body: ElodinAST
      ): EloVal = ???

      override def visitApply(
        func: ElodinAST,
        args: List[ElodinAST]
      ): EloVal = ???
    })
    ???
  }

}
