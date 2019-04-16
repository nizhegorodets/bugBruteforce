import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

case class Maze(width: Int, height: Int) {
  private var matrix = Array.ofDim[Int](height, width)
  private val wall = 1
  private val notWall = 0
  private val sourceCell = Cell(1,1)
  private val destCell = Cell(width - 2, height - 2)
  var visited = Array.fill[Boolean](width * height)(false)

  def printMatrix: Unit = {
    matrix foreach { row => row foreach print; println }
  }

  def buildWalls: Unit = {
    for(i <- 0 until width){
      matrix(0)(i) = wall
      matrix(height - 1)(i) = wall
    }

    for(i <- 0 until height){
      matrix(i)(0) = wall
      matrix(i)(width - 1) = wall
    }
  }

  def reachability: Boolean = {
    var cellsForCheck = getNeighbors(sourceCell)
    var ans = false
    breakable {
      while (cellsForCheck.nonEmpty) {
        if (cellsForCheck.contains(destCell)) {
          ans = true
          break
        }
        val currCell = cellsForCheck(cellsForCheck.size - 1)
        cellsForCheck.remove(cellsForCheck.size -1)
        cellsForCheck.++=(getNeighbors(currCell))
      }
    }
    // reset visited
    visited = Array.fill[Boolean](width * height)(false)
    ans
  }

  def getNeighbors(index : Cell): ArrayBuffer[Cell] ={
    visited(index.heightShift * width + index.widthShift) = true
    val neighbors = new ArrayBuffer[Cell](0)
    if (index.widthShift > 1)
      neighbors += Cell(index.widthShift - 1, index.heightShift)
    if (index.heightShift > 1)
      neighbors += Cell(index.widthShift, index.heightShift - 1)
    if (index.widthShift < width - 2)
      neighbors += Cell(index.widthShift + 1, index.heightShift)
    if (index.heightShift < height - 2)
      neighbors += Cell(index.widthShift, index.heightShift + 1)

    var ans = new ArrayBuffer[Cell](0)
    // delete cell-wall
    for(element <- neighbors){
      if((matrix(element.heightShift)(element.widthShift) == 0)
        && (!visited(element.heightShift * width + element.widthShift))){
        ans += element
      }
    }
    ans
  }

  def inc: Boolean = {
    var ans = true
    var curPos = width - 3
    var curLine = height - 2

    breakable {
      while (true) {
        if (matrix(curLine)(curPos) == notWall) {
          matrix(curLine)(curPos) = wall
          break
        }
        else {
          matrix(curLine)(curPos) = notWall
          curPos -= 1
          if (curPos < 1){
            curPos = width - 2
            curLine -= 1
          }
          if ((curLine == 1) && (curPos == 1)) {
            println("finish")
            ans = false
            break
          }
        }
      }
    }
    ans
  }
}

object Maze {
  def apply(width: Int, height: Int): Maze = {
    val ans = new Maze(width, height)
    ans.buildWalls
    ans
  }
}
