import scala.util.control.Breaks.{break, breakable}

object Main extends App {
  val mz = Maze()
  mz.generateRandomMaze
  while(mz.inc) {
    mz.inc
  }
}
