import scala.collection.mutable.ListBuffer

case class CombinationAlg(maze: Maze) extends Algorithm {
  val a = new ListBuffer[Int]()
  var lastCombination = new ListBuffer[Cell]()
  var n = 1
  var iter = a.combinations(n)

  def this(maze: Maze, inN: Int) {
    this(maze: Maze)
    for(i <- 2 to 550) {
      a += i
    }
    n = inN
    iter = a.combinations(n)
  }

  def inc: Boolean = {
    if(lastCombination.nonEmpty){
      for(cell <- lastCombination)
        maze.Matrix(cell.widthShift)(cell.heightShift) = 0
      lastCombination.clear()
    }

    if (iter.hasNext) {
      val combination = iter.next()

        for(index <- combination) {
          val curCell = convertIndexToCell(index)
          maze.Matrix(curCell.widthShift)(curCell.heightShift) = mazeProperties.wall
          lastCombination += curCell
          //println(s"$index $curCell")
        }
      true
    }
    else {
      false
    }
  }

  def convertIndexToCell(index: Int): Cell = {
    if(index % (mazeProperties.width - 2) == 0){
      Cell(index / (mazeProperties.width - 2), mazeProperties.width - 2)
    }
    else {
      Cell(index / (mazeProperties.width - 2) + 1, index % (mazeProperties.width - 2))
    }
  }
}


