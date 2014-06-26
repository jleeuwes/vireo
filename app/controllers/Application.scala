package controllers

import models._

import play.api._
import play.api.mvc._
import play.api.libs.json._ // JSON library

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }
  
  // http://www.playframework.com/documentation/2.1.x/ScalaJsonRequests
  // http://www.playframework.com/documentation/2.3.x/ScalaJsonHttp
  // http://www.playframework.com/documentation/2.3.x/ScalaJson
  def sudokuSubmit = Action(parse.json) { request =>
    val sudokuResult = request.body.validate[SudokuSubmit]
    sudokuResult.fold(
      errors => {
        BadRequest(JsError.toFlatJson(errors))
      }
    , sudokuSubmit => {
        val sudoku = sudokuSubmit.toSudoku(1,9)
        val changes = sudoku.visibleFrom(sudokuSubmit.row, sudokuSubmit.column).map
          { case (r,c) =>
            Json.obj("row" -> r, "col" -> c, "possible" -> sudoku.possibilities(r,c))
          }

        print(changes)
        Ok(Json.toJson(changes))
      }
    )
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(
      controllers.routes.javascript.Application.sudokuSubmit
    ));
  }
  
}
