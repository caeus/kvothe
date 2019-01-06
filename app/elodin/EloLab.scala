package elodin

import cats.Eval
import elodin.EloLab._
import elodin.ElodinAST.EloVisitor

object EloLab {

  sealed trait EloVal
  sealed trait EloFixedVal extends EloVal
  sealed trait  EloCallable extends EloVal{
    def apply(args: Seq[EloVal]): EloVal
  }
  case class EloLazyVal(eval: Eval[EloVal]) extends EloVal
  case class EloNumber(value: BigDecimal) extends EloFixedVal
  case class EloGeneric[R](value: R) extends EloFixedVal
  case class EloNull() extends EloFixedVal
  case class EloArray(values: Seq[EloVal]) extends EloCallable{

  }

  case class EloObject(fields: Map[String, EloVal]) extends EloCallable{

  }
  case class EloError(msg: String) extends EloFixedVal
  case class EloLambda(
    argNames: Seq[String],
    creationScope: EloScope,
    body: ElodinAST
  ) extends EloCallable {
    def apply(args: Seq[EloVal]): EloVal = {
      if (args.size != argNames.size)
        EloError(s"function takes ${argNames.size} parameters, but it was called with ${args.size} parameters")
      else {
        val scope = ChildEloScope(argNames.zip(args).toMap, creationScope)
        body.accept(new ExecutorVisitor(scope))
      }
    }
  }

  sealed trait EloScope {
    def get(name: String): EloVal
  }
  object EloRootScope extends EloScope {
    override def get(name: String): EloVal = new EloNull
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
  override def visitString(value: String): EloVal = EloGeneric(value)

  override def visitNumber(value: BigDecimal): EloVal = EloGeneric(value)

  override def visitNull: EloVal = EloNull()

  override def visitBoolean(value: Boolean): EloVal = EloGeneric(value)

  override def visitObject(fields: Map[String, ElodinAST]): EloVal = EloObject(fields.mapValues(_.accept(thisVisitor)))

  override def visitArray(items: Seq[ElodinAST]): EloVal = EloArray(items.map(_.accept(thisVisitor)))

  override def visitRef(ref: String): EloVal = scope.get(ref)

  override def visitLet(
    bindings: Map[String, ElodinAST],
    body: ElodinAST
  ): EloVal = {
    val asd: EloScope = ChildEloScope(bindings.mapValues { exp =>
      EloLazyVal(Eval.later(exp.accept(new ExecutorVisitor(asd))))
    }, scope)
    body.accept(new ExecutorVisitor(asd))
  }

  override def visitLambda(
    args: Seq[String],
    body: ElodinAST
  ): EloVal = EloLambda(args: Seq[String], creationScope = scope, body)

  override def visitApply(
    func: ElodinAST,
    args: List[ElodinAST]
  ): EloVal = ???
}
