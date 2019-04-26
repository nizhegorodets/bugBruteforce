object Main extends App {
  val mz = Maze()

  var record = 0
  while(mz.inc){
    val steps = mz.runCheck
    if(steps > record){
      record = steps
      mz.printHumanly
      println(steps)
      println("---")
      mz.printToFile(steps.toString)
    }
  }
}
