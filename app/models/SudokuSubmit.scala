package models

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

// Class for easily working with the JSON data the client sends us.

case class SudokuSubmit(table: Vector[Vector[Int]], row: Int, column: Int, value: Int) {
  def toSudoku(min: Int, max:Int) : Sudoku = {
    Sudoku.create(min, max, table.flatten)
  }
}

// http://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators

object SudokuSubmit {
  implicit val submitReads: Reads[SudokuSubmit] = (
      (JsPath \ "table").read[Vector[Vector[Int]]] and
      (JsPath \ "row").read[Int] and
      (JsPath \ "column").read[Int] and
      (JsPath \ "value").read[Int]
    )(SudokuSubmit.apply _)
}
