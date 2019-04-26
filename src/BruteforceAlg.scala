import scala.util.control.Breaks.{break, breakable}

case class BruteforceAlg(maze: Maze) extends Algorithm {
  def inc: Boolean = {
    var ans = true
    var curPos = mazeProperties.width - 3
    var curLine = mazeProperties.height - 2

    breakable {
      while (true) {
        if (maze.Matrix(curLine)(curPos) == mazeProperties.notWall) {
          maze.Matrix(curLine)(curPos) = mazeProperties.wall
          break
        }
        else {
          maze.Matrix(curLine)(curPos) = mazeProperties.notWall
          curPos -= 1
          if (curPos < 1){
            curPos = mazeProperties.width - 2
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
