import scala.collection.mutable.ListBuffer

case class DeletingAlg(maze: Maze) extends Algorithm {
  var a = new ListBuffer[Int]()
  var lastCombination = new ListBuffer[Cell]()
  var n = 1
  var iter = a.combinations(n)

  def this(maze: Maze, inN: Int, cells: ListBuffer[Cell]) {
    this(maze: Maze)
    setIndexes(cells)
    n = inN
    iter = a.combinations(n)
  }

  def setIndexes(cells : ListBuffer[Cell]): Unit = {
    a.clear()
    cells.foreach {a += convertCellToIndex(_)}
    //a = scala.util.Random.shuffle(a)
  }

  def inc: Boolean = {
    if(lastCombination.nonEmpty){
      for(cell <- lastCombination)
        maze.Matrix(cell.widthShift)(cell.heightShift) = mazeProperties.wall
      lastCombination.clear()
    }

    if (iter.hasNext) {
      val combination = iter.next()

      for(index <- combination) {
        val curCell = convertIndexToCell(index)
        // check existing wall
        if (maze.Matrix(curCell.widthShift)(curCell.heightShift) == mazeProperties.wall) {
          maze.Matrix(curCell.widthShift)(curCell.heightShift) = mazeProperties.notWall
          lastCombination += curCell
        }
        //println(s"$index $curCell")
      }
      true
    }
    else {
      false
    }
  }

  def convertIndexToCell(index: Int): Cell = {
    //println(index)
    if(index % (mazeProperties.width - 2) == 0){
      Cell(index / (mazeProperties.width - 2), mazeProperties.width - 2)
    }
    else {
      Cell(index / (mazeProperties.width - 2) + 1, index % (mazeProperties.width - 2))
    }
  }

  def convertCellToIndex(cell: Cell): Int = {
    //println(s"cell = $cell")
    //println((cell.widthShift - 1) * (mazeProperties.width - 2) + cell.heightShift)
    (cell.widthShift - 1) * (mazeProperties.width - 2) + cell.heightShift
  }
}


