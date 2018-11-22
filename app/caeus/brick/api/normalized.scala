//package caeus.brick.api
//
//import scala.concurrent.Future
//import scala.language.higherKinds
//
//
//trait Transformer[T, R] {
//  def apply(input: T): R
//}
//
//trait ToOutput[T, Format] {
//  def apply(output: T): Format
//}
//
//trait FromInput[Format, T] {
//  def apply(input: Format): T
//}
//
//
//trait NormalTypeDef {
//  def name: String
//}
//
//
//
//trait NormalFields[From, Format] {
//  def value: Map[String, Req[From, Seq[Format]] => Future[Format]]
//}
//
//
//
//
//object NormalFields {
//  //StructField[Eff[_], From, Args, To]
//  implicit def singleField[Eff[_], From, Args, To, Format]()
//  : Transformer[Field[Eff,From,Args,To],NormalFields[From,Format]] = (field: Field[Eff, From, Args, To]) => new NormalFields[From, Format] {
//    override def value: Map[String, Req[From, Seq[Format]] => Future[Format]] = Map(field.name->{
//      req:Req[From, Seq[Format]] =>
//        val nReq:Req[From,Args]= ???
//        field.resolver
//    })
//  }
//
//}