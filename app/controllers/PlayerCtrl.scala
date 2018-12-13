package controllers

import scala.concurrent.Future
import scala.language.higherKinds

import edu.caeus.herbivicus.sower.data.{Request => SRequest}
import edu.caeus.herbivicus.vine.VineT
import javax.inject.Inject
import kvothe.Ctx
import kvothe.api.PlayerApi
import kvothe.domain.UserId
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsValue
import play.api.mvc._
import trees.PlayerSprout

class PlayerCtrl @Inject()(
  ctx: Ctx,
  cc: ControllerComponents
)(implicit scheduler: Scheduler) extends AbstractController(cc) {


  def withUser[B](block: PlayerApi => B => Task[Result]): Request[AnyContent] => Future[Result] = ???


  //  withUser{api => _ =>
  //    val asd = Chain[Task,PlayerApi[Task]](api).down("sheets")(_.sheets)
  //    asd.down("")(_.entries)
  //    ???
  //  }

  val playerId = UserId("caeus")
  val playerApi = PlayerApi(ctx, playerId)

  def process(route: String): Action[JsValue] = Action.async(parse.json) {
    request =>
      PlayerSprout.schema.compile(SRequest(route.split('/').toList,
        request.queryString.toList.flatMap {
          case (key, values) => values.map {
            value => key -> value
          }
        }, request.body))
        .map {
          program =>
            program.run(VineT.pure(playerApi))
              .runAsync
        }.fold(Future.failed, identity)
        .map {
          body => Ok(body)
        }
  }

}
