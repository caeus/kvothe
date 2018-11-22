//package caeus.brick.api
//
//import scala.concurrent.Future
//import scala.language.higherKinds
//
//import shapeless._
//
//
//trait Req[From, Args] {
//  def from: From
//  def args: Args
//
//}
//
//trait TypeDef {
//  def name: String
//}
//
//sealed trait Fields[Eff[_], From]{
//
//}
//object Fields{
//  implicit class StructFieldsOps[Eff[_],From,Fields<:Fields[Eff,From]](val value:Fields){
//    def &[ArgsL, ToL](other: Field[Eff, From, ArgsL, ToL]): FieldProduct[Eff, From, Fields, ArgsL, ToL] ={
//      FieldProduct(value,other)
//    }
//  }
//
//}
//
//object asd{
//  val field:Field[Future,Unit,Unit,Unit] = ???
//  val field1:Field[Future,Unit,Int,Int] = ???
//  field & field1
//
//}
//case class StructDef[Eff[_], From, Fields <: Fields[Eff, From]](
//  name: String,
//  fields: Lazy[Fields]
//) extends TypeDef
//
//
//case class FieldProduct[Eff[_], From, Init <: Fields[Eff, From], ArgsL,ToL](
//  init: Init,
//  last: Field[Eff,From,ArgsL,ToL]
//)
//
//case class Field[Eff[_], From, Args, To](
//  name: String,
//  resolver: Req[From, Args] => Eff[To]
//) extends Fields[Eff, From] {
//
//}
