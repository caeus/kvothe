package trees

import scala.util.Try

import edu.caeus.herbivicus.sower.Pipe
import kvothe.api.PlayerApi
import kvothe.domain.{CreateSheetRequest, SheetId, UpdateSheetRequest}
import kvothe.sower.KvotheSower
import kvothe.sower.KvotheSower.dsl._
import kvothe.utility.json.{KvotheReaders, KvotheWriters}
import monix.eval.Task
import play.api.libs.json.{JsValue, Reads, Writes}


object PlayerSprout extends KvotheWriters with KvotheReaders {

  implicit def asdas[T](implicit tWrites: Writes[T]): Pipe[T, JsValue] = Pipe[T, JsValue] {
    t => tWrites.writes(t)
  }

  implicit def kajhdkajhsdj[T](implicit tReads: Reads[T]): Pipe[JsValue, Try[T]] = Pipe[JsValue, Try[T]] {
    value =>
      Try(tReads.reads(value).get)
  }
  implicitly[Pipe[JsValue,Try[CreateSheetRequest]]]
  //kajhdkajhsdj(asd)

  val schema = fork[PlayerApi](
    _.branch("sheets") { _ =>
      api => Task eval api.sheets
    }.into(fork(
      _.branch("entries"){
        _ => _.entries
      }.into(leaf),
      _.branch("create")
        .body(as[CreateSheetRequest]) { req =>
          api=>api.create(req.body)
        }.into(leaf),
      _.branch("one")(---)
        .into(fork(_.branch(Segment.map(_.map(SheetId))) { req =>
          _.one(req.segment)
        }.into(option of fork(
          _.branch("versions") { _ =>
            _.versions
          }.into(leaf),
          _.branch("versioned")(---)
            .into(fork(
              _.branch(Segment) { req =>
                _.versioned(Some(req.segment)
                  .filter(_ != "current"))
              }.into(option of fork(
                _.branch("data")
                (_ => _.data)
                  .into(leaf),
                _.branch("changelog")
                (_ => _.changelog)
                  .into(leaf)
              ))
            )),
          _.branch("update")
            .body(kajhdkajhsdj(implicitly[Reads[UpdateSheetRequest]])) { req =>
              _.update(req.body)
            }
            .into(leaf)
        )))
        ))
    ))

}



