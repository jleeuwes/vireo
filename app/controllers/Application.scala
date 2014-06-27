package controllers

import models._

import play.api._
import play.api.mvc._
import play.api.libs.json._

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
        // Create a 9x9 sudoku from the data received,
        val sudoku = sudokuSubmit.toSudoku(1,9)
        
        // and generate the changes to the possibilities in each cell that is
        // visible from the changed cell.
        val changes = sudoku.visibleFrom(sudokuSubmit.row, sudokuSubmit.column).map
          { case (r,c) =>
            Json.obj("row" -> r, "column" -> c, "possible" -> sudoku.possibilities(r,c))
          }

        Ok(Json.toJson(changes))
      }
    )
  }
  
}
