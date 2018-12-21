package io.sower

import scala.language.higherKinds

abstract class Pot[From, F[_], Input, Output](val sower: Sower[F, Input, Output] with SowerDSL[F,Input,Output]) {

  val schema: sower.Sprout[From]

  def apply(req: sower.NormalReq): sower.Sync[sower.Program[From]] = schema.compile(req)
}
