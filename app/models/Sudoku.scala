package models

// def catOptions [A, T[_]] (xs: Traversable[Option[A]])(implicit cast:
// Traversable[A] => T[A]) : T[A] = {
// def catOptions [A] (xs: Traversable[Option[A]]) : Traversable[A] = {
//   xs.filter(_.nonEmpty).map(_.get)
// }

class Sudoku (symbols: Set[Int], values: Vector[Option[Int]]) {
  val nrOfSymbols         = symbols.size
  private val blockSideD  = scala.math.sqrt(nrOfSymbols)
  val blockSide           = blockSideD.toInt
  val requiredTableSize   = nrOfSymbols * nrOfSymbols
  
  // // These store possibilities left per column/row/block.
  // // This seemed like a good idea and should be faster (when mass-querying
  // // possibilities), but isn't.
  // val unit = (0 to nrOfSymbols - 1)
  // lazy val allColumns = unit.map { col =>
  //   symbols -- catOptions(unit.map { row => table(index(row, col)) })
  // }
  // lazy val allRows = unit.map { row =>
  //   symbols -- catOptions(unit.map { col => table(index(row, col)) })
  // }
  // val smallUnit = (0 to blockSide - 1)
  // val blocks = for (a <- smallUnit; b <- smallUnit) yield (a,b)
  // lazy val allBlocks = blocks.map { case (bRow, bCol) =>
  //   val row0 = bRow * blockSide
  //   val col0 = bCol * blockSide
  //   symbols -- catOptions(blocks.map { case (row, col) =>
  //     table(index(row0 + row, col0 + col))
  //   })
  // }

  if (blockSide != blockSideD) {
    throw new IllegalArgumentException("nrOfSymbols should be a square")
  }

  if (values.size > requiredTableSize) {
    throw new IllegalArgumentException("too many values")
  }
  val table = values.padTo(requiredTableSize, None)

  private def checkCoor(what: String, i: Int) : Int = {
    if (i < 0 || i > nrOfSymbols) {
      throw new IndexOutOfBoundsException(what + "=" + i.toString)
    } else {
      return i
    }
  }

  /* private */ def index(row: Int, col: Int) : Int = {
    checkCoor("row", row)
    checkCoor("col", col)

    row * nrOfSymbols + col
  }

  def toRow(index: Int)     : Int = index / nrOfSymbols
  def toColumn(index: Int)  : Int = index % nrOfSymbols
  def toBlock(index: Int)   : Int = toBlock(toRow(index), toColumn(index))
  def toBlock(row: Int, col: Int) : Int
    = row / blockSide * blockSide + col / blockSide

  // def possibilities_(row: Int, col: Int) : Set[Int] = {
  //   allRows(row) & allColumns(col) & allBlocks(toBlock(index(row, col)))
  // }

  def allCells() = {
    table.indices.map ( i => (toRow(i), toColumn(i)) )
  }
  
  // Return all cell indices that are visible from the given (row, col)
  def visibleFrom(row: Int, col: Int, includeSelf: Boolean = true) = {
    val block = toBlock(row, col)

    allCells().filter { case (row2, col2) =>
      (col == col2 || row == row2 || block == toBlock(row2, col2)) &&
      (includeSelf || !(col == col2 && row == row2))
    }
  }

  def possibilities(row: Int, col: Int) : Set[Int] = {
    var poss  = symbols

    visibleFrom(row, col, false).foreach { case (row2, col2) =>
      // Remove the value in cell there from possibilities.
      table(index(row2, col2)).foreach { value => poss = poss - value }

      // This would be a more clear but more verbose way:
      // table(there) match {
      //   case Some(value) => poss = poss - value
      //   case None =>  // ugly no-op
      // }
    }

    return poss
  }
}

object Sudoku {
  // Convenience function. Create a sudoku by specifying min and max symbol and
  // a Vector in which cells are just an Int and empty cells are 0.
  def create(min: Int, max: Int, values: Vector[Int]): Sudoku = {
    return new Sudoku((min to max).toSet, values.map { n =>
      n match {
        case 0 => None
        case n => Some(n)
      }
    })
  }

}
