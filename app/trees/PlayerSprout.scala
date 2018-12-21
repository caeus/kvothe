package trees

import scala.util.Try

import io.sower.{Pipe, Pot}
import kvothe.api.PlayerApi
import kvothe.domain.{CreateSheetRequest, SheetId, UpdateSheetRequest}
import kvothe.sower.{KvotheReq, KvotheSower}
import kvothe.utility.json.{KvotheReaders, KvotheWriters}
import monix.eval.Task
import play.api.libs.json.{JsValue, Reads, Writes}


object PlayerSprout extends Pot[PlayerApi, Task, KvotheReq, JsValue](KvotheSower) with KvotheWriters with KvotheReaders {
  import sower.syntax._

  implicit def kvotheReqToMarshaller[T](implicit tWrites: Writes[T]): Pipe[T, JsValue] = Pipe[T, JsValue] {
    t =>
      tWrites.writes(t)
  }

  implicit def kvotheReqToUnmarshaller[T](implicit tReads: Reads[T]): Pipe[KvotheReq, Try[T]] = Pipe[KvotheReq, Try[T]] {
    value =>
      Try(value.body.as[T])
  }


  val schema: PlayerSprout.sower.Sprout[PlayerApi] = fork[PlayerApi] {
    _.branch("sheets") { _ =>
      api => Task eval api.sheets
    }.into(fork {
      _.branch("entries") {
        _ => _.entries
      }.into(leaf)
        .branch("create")
        .payload(as[CreateSheetRequest]) { req =>
          api => api.create(req.payload)
        }.into(leaf)
        .branch("one")
        .segment(asString.map(_.map(SheetId(_)))) {
          req => _.one(req.segment)
        }.into(option of fork {
        _.branch("versions") { _ =>
          _.versions
        }.into(leaf)
          .branch("versioned")
          .segment(asString) {
            req => _.versioned(Some(req.segment).filter(_ != "current"))
          }.into(option of fork {
          _.branch("data")(_ => _.data)
            .into(leaf)
            .branch("changelog")(_ => _.changelog)
            .into(leaf)
        })
          .branch("update")
          .payload(as[UpdateSheetRequest]) { req =>
            _.update(req.payload)
          }.into(leaf)
      })
    })
  }
}



