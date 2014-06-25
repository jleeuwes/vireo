class Sudoku (symbols: Set[Int], values: Vector[Option[Int]]) {
  val nrOfSymbols         = symbols.size
  private val blockSideD  = scala.math.sqrt(nrOfSymbols)
  val blockSide           = blockSideD.toInt
  val requiredTableSize   = nrOfSymbols * nrOfSymbols

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
  def toBlock(index: Int)   : Int = toRow(index) / blockSide * blockSide + toColumn(index) / blockSide

  def possibilities(row: Int, col: Int) : Set[Int] = {
    val here  = index(row, col)
    val block = toBlock(here)
    var poss  = symbols

    table.indices.foreach { there =>
      if (  col    == toColumn(there)
         || row    == toRow(there)
         || block  == toBlock(there)
      ) {
        // Remove the value in cell there from possibilities.
        table(there).foreach { value => poss = poss - value }
        // This would be a more clear but more verbose way:
        // table(there) match {
        //   case Some(value) => poss = poss - value
        //   case None =>  // ugly no-op
        // }
      }
    }

    return poss
  }
}
