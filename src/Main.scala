object Main extends App {
  val width = 6
  val height = 5

  val mz = Maze(width, height)
  //mz.buildWalls
  var count = 0
  while(mz.inc){
    mz.printMatrix
    println(mz.reachability)
    println("---")
    count+=1
    Thread.sleep(500)
  }
  println(count)
}
