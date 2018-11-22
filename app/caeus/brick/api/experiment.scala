package caeus.brick.api

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import io.circe.Json

trait Rep

trait Invocation {
  def arguments: Map[String, Json]
  def projection: Option[Projection]
}

trait Projection {
  def invocations: Map[String, Invocation]
}


trait Field {
  def name: String
  def arguments: Map[String, TypeDef]
  def returnType: TypeDef
  def resolve(invocation: Invocation): Future[Rep]
  def invoke(invocation: Invocation): Future[Json] = {
    resolve(invocation).flatMap {
      rep =>
        returnType.process(rep, invocation.projection)
    }
  }
}


trait TypeDef {
  def name: String

  def process(
    from: Rep,
    projection: Option[Projection]
  ): Future[Json]

}

trait ObjectTypeDef extends TypeDef {
  override def name: String

  def fields: Map[String, Field]

  def join(map: Map[String, Future[Json]]): Future[Json]

  override def process(from: Rep, projection: Option[Projection]): Future[Json] = {
    projection.map { proj =>
      proj.invocations.map {
        case (field, invocation) =>
          field -> fields(field).invoke(invocation)
      } match {
        case res => join(res)
      }
    }.getOrElse(Future.successful(Json.Null))
  }
}