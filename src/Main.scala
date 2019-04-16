object Main extends App {
  val width = 13
  val height = 5

  val mz = Maze(width, height)
  mz.buildWalls
  var count = 0
  while(mz.inc){
    //mz.printMatrix
    //println("---")
    count+=1
  }
  println(count)
}
