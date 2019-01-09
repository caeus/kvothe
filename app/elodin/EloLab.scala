package elodin

import cats.Eval
import elodin.EloLab._
import elodin.ElodinAST.EloVisitor

object EloLab {

  sealed trait EloVal

  case class EloEager[X](x:X) extends EloVal

  sealed trait EloScope {
    def get(name: String): EloVal
  }
  object EloRootScope extends EloScope {
    override def get(name: String): EloVal = ???
  }
  case class ChildEloScope(
    bindings: Map[String, EloVal],
    parent: EloScope
  ) extends EloScope {
    override def get(name: String): EloVal = bindings.getOrElse(name, parent.get(name))
  }

  def run(ast: ElodinAST): Eval[Any] = {
    //ast.accept()
    ???
  }

}
class ExecutorVisitor(scope: EloScope) extends EloVisitor[EloVal] {
  thisVisitor =>
  override def visitString(value: String): EloVal = ???

  override def visitNumber(value: BigDecimal): EloVal = ???

  override def visitNull: EloVal = ???

  override def visitBoolean(value: Boolean): EloVal = ???

  override def visitObject(fields: Map[String, ElodinAST]): EloVal = ???

  override def visitArray(items: Seq[ElodinAST]): EloVal = ???

  override def visitRef(ref: String): EloVal = scope.get(ref)

  override def visitLet(
    bindings: Map[String, ElodinAST],
    body: ElodinAST
  ): EloVal = ???

  override def visitLambda(
    args: String,
    body: ElodinAST
  ): EloVal = ???

  override def visitApply(
    func: ElodinAST,
    args: List[ElodinAST]
  ): EloVal = ???
}
