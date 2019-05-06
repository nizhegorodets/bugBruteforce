

case class DeepMutationAlg(maze: Maze) extends Algorithm {
  var addingAlg = new CombinationAlg(maze, 1, maze.getAllNotWallCells)
  var deletingAlg = new DeletingAlg(maze, 1, maze.getAllWallCells)
  var record = 0
  val maxDeep = 5

  def inc: Boolean = {
    var combinations = 1

    while (combinations <= maxDeep) {
      while (addingAlg.inc) {
        val steps = maze.runCheck

        if (steps > record) {
          record = steps
          println("cells = " + maze.getAllWallCells.size.toString + " record = " + record.toString)
          addingAlg = new CombinationAlg(maze, combinations, maze.getAllNotWallCells)
          combinations = 1
          maze.printToFile(steps.toString)
        }
      }

      println("switch to deleting alg")
      while (deletingAlg.inc) {
        val steps = maze.runCheck

        if (steps >= record) {
          record = steps
          println("cells = " + maze.getAllWallCells.size.toString + " record = " + record.toString)
          deletingAlg = new DeletingAlg(maze, combinations, maze.getAllWallCells)
          combinations = 1
          maze.printToFile(steps.toString)
        }
      }
      combinations += 1
      println(s"combinations = $combinations")
      addingAlg = new CombinationAlg(maze, combinations, maze.getAllNotWallCells)
      deletingAlg = new DeletingAlg(maze, combinations, maze.getAllWallCells)
    }
    false
  }
}
