package kvothe.rost

import scala.language.higherKinds
import scala.util.{Failure, Try}

trait Schema[F[_], In, Format] {
  //Fixme body type should be something that allows laziness of reading
  final def handle(ctx: NormalCtx[In]): Try[F[Format]] = {
    compile(ctx.req).map{
      executor=>
        executor.run(ctx.value)
    }
  }

  def compile(req: NormalReq): Try[CompiledAction[F, In, Format]]

  //Fixme
  def description: Unit
}

object Schema {
  def apply[F[_], In, Format](paths: (PathBuilder[F, In, Format] => Property[F, In, Format])*): Schema[F, In, Format] = ???
}

private[rost] class ObjSchema[F[_], In, Format](
  props: Seq[Property[F, In, Format]]
) extends Schema[F, In, Format] {
  override def description: Unit = ???

  override def compile(req: NormalReq): Try[CompiledAction[F, In, Format]] = {
    props.iterator.map(_.compile(req)).dropWhile(_.isEmpty).map(_.get).toIterable.headOption
      .fold(Failure(new Exception("No prop matched path")): Try[CompiledAction[F, In, Format]])(identity)
  }
}