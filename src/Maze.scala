import util.control.Breaks._

case class Maze(width: Int, height: Int) {
  private var matrix = Array.ofDim[Int](height, width)
  private val wall = 1
  private val notWall = 0

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
