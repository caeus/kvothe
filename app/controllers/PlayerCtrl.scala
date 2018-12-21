package controllers

import scala.concurrent.Future
import scala.language.higherKinds

import cats.MonadError
import io.sower.{CompileError, Request => SRequest}
import io.vine.VineT
import javax.inject.Inject
import kvothe.Ctx
import kvothe.api.PlayerApi
import kvothe.domain.{PlayerCompileError, UserId}
import kvothe.sower.KvotheReq
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.{JsNull, Json}
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

  MonadError

  val playerId = UserId("caeus")
  val playerApi = PlayerApi(ctx, playerId)

  def process(route: String): Action[AnyContent] = Action.async {
    request =>
      PlayerSprout(SRequest(route.split('/').toList,
        KvotheReq(request.queryString, request.body.asJson.getOrElse(JsNull))))
        .map {
          program =>
            program.run(VineT.pure(playerApi))
              .runAsync
        }.fold(Future.failed, identity)
        .map {
          body =>
            Ok(body)
        }.recover {
        case e: CompileError =>
          Ok(Json.toJson(PlayerCompileError(e.getMessage, remainingRoute = e.request.route, fullRoute = e.request.original)))
      }
  }

}
