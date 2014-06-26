package models

// http://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators

import play.api.libs.json._ // JSON library
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

case class SudokuSubmit(table: Vector[Vector[Int]], row: Int, column: Int, value: Int) {
  def toSudoku(min: Int, max:Int) : Sudoku = {
    Sudoku.create(min, max, table.flatten)
  }
}

object SudokuSubmit {
  implicit val submitReads: Reads[SudokuSubmit] = (
      (JsPath \ "table").read[Vector[Vector[Int]]] and
      (JsPath \ "row").read[Int] and
      (JsPath \ "column").read[Int] and
      (JsPath \ "value").read[Int]
    )(SudokuSubmit.apply _)
}
