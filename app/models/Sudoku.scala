package models

class Sudoku(
  // The set of symbols used (normally 1 through 9).
  symbols: Set[Int],

  // A flat Vector of all cell values.
  // Empty cells are None, filled cells are Some(value).
  values: Vector[Option[Int]]
) {

  // == Initialization ===

  // In a regular sudoku, there are 9 symbols,
  val nrOfSymbols         = symbols.size

  // the side of a block has size 3,
  private val blockSideD  = scala.math.sqrt(nrOfSymbols)
  val blockSide           = blockSideD.toInt

  // and there are 9x9 = 81 cells in total.
  val requiredTableSize   = nrOfSymbols * nrOfSymbols

  // Check for irregular sudokus.
  if (blockSide != blockSideD) {
    throw new IllegalArgumentException("nrOfSymbols should be a square")
  }
  
  // Pad the specified values to the exact number of cells.
  if (values.size > requiredTableSize) {
    throw new IllegalArgumentException("too many values")
  }
  val table = values.padTo(requiredTableSize, None)

  
  // == Function definitions ===

  // Convert from 2d coordinates to flat Vector index.
  private def index(row: Int, col: Int) : Int = {
    checkCoor("row", row)
    checkCoor("col", col)

    row * nrOfSymbols + col
  }
  
  // Bounds checker.
  private def checkCoor(what: String, i: Int) = {
    if (i < 0 || i > nrOfSymbols) {
      throw new IndexOutOfBoundsException(what + "=" + i.toString)
    } else {
      i
    }
  }

  // Convert from flat index to row or column.
  private def toRow(index: Int) : Int
    = index / nrOfSymbols
  private def toColumn(index: Int) : Int
    = index % nrOfSymbols

  // Get the block number in which the specified cell lives.
  private def toBlock(row: Int, col: Int) : Int
    = row / blockSide * blockSide + col / blockSide
  
  def allCells() = {
    table.indices.map { i => (toRow(i), toColumn(i)) }
  }
  
  // Return all cell indices that are visible from the given (row, col)
  def visibleFrom(row: Int, col: Int, includeSelf: Boolean = true) = {
    val block = toBlock(row, col)

    allCells().filter { case (row2, col2) =>

      // The other cell should share a coordinate with this cell:
      (col == col2 || row == row2 || block == toBlock(row2, col2)) &&
      // And it should not be the same cell, unless we want to include ourselves:
      (includeSelf || !(col == col2 && row == row2))
    }
  }
  
  // Return all possible values of the given row and column.
  def possibilities(row: Int, col: Int) = {
    // We start with all symbols.
    var possible = symbols
    
    // The values of every visible cell are not possible for this cell.
    visibleFrom(row, col, false).foreach { case (row2, col2) =>

      // Remove the value in the other cell from the possibilities.
      // Option.foreach fires only if the cell contains Some value. If there is
      // None, we do nothing.
      table(index(row2, col2)).foreach { value => possible = possible - value }

    }

    possible
  }
}

object Sudoku {
  // Create a sudoku by specifying min and max symbol and a Vector of Ints.
  // Empty cells are 0.
  def create(min: Int, max: Int, values: Vector[Int]) = {
    val symbols = (min to max).toSet

    new Sudoku(symbols, values.map { n =>
      n match {
        case 0 => None
        case n => Some(n)
      }
    })
  }

}
