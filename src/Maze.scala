import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import util.control.Breaks._
import scala.math.min
import java.io._
import java.nio.charset.StandardCharsets


case class Maze() {
  var matrix = Array.ofDim[Int](mazeProperties.height, mazeProperties.width)
  private val alg = new maskAlg(this)
  private val sessionID = scala.util.Random.nextInt(1000000000).toString

  var visited = Array.fill[Boolean](mazeProperties.width * mazeProperties.height)(false)

  def printToFile(steps: String): Unit = {
    val r = scala.util.Random

    val dir = new File(s"records/$sessionID").mkdir()
    val random = r.nextInt(1000000000)
    val fileName = s"records/$sessionID/$steps-" + random.toString
//    val file = new File(fileName)
//    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8))
//
//    for(i <- 0 until mazeProperties.height){
//      for(j <- 0 until mazeProperties.width){
//        if(matrix(i)(j) == mazeProperties.wall)
//          bw.write("1")
//        else
//          bw.write("0")
//      }
//      bw.newLine()
//    }
//    bw.close()

    //draw image
    val drawer = new Drawer(this, random, fileName)
    drawer.draw
  }

  def printMatrix: Unit = {
    matrix foreach { row => row foreach print; println }
  }

  def generateRandomMaze: Unit = {
    var isReachability = false

    while(!isReachability) {
      val r = scala.util.Random
      val numberWalls = mazeProperties.minRandom + r.nextInt(mazeProperties.maxRandom - mazeProperties.minRandom)
      var cells = getAllNotWallCells
      cells = scala.util.Random.shuffle(cells).take(numberWalls)
      clearMatrix

      for (i <- cells) {
        matrix(i.widthShift)(i.heightShift) = mazeProperties.wall
      }
      isReachability = reachability
    }
    printHumanly
  }

  def clearMatrix: Unit = {
    for (i <- 1 to mazeProperties.height - 2) {
      for (j <- 1 to mazeProperties.width - 2) {
        matrix(i)(j) = mazeProperties.notWall
      }
    }
  }

  def getAllNotWallCells: ListBuffer[Cell] = {
    var ans = new ListBuffer[Cell]()

    for (i <- 1 to mazeProperties.height - 2) {
      for (j <- 1 to mazeProperties.width - 2) {
        if(matrix(i)(j) == mazeProperties.notWall) {
          ans += Cell(i, j)
          //println(Cell(i, j))
        }
      }
    }
    ans
  }

  def getAllWallCells: ListBuffer[Cell] = {
    var ans = new ListBuffer[Cell]()

    for (i <- 1 to mazeProperties.height - 2) {
      for (j <- 1 to mazeProperties.width - 2) {
        if(matrix(i)(j) == mazeProperties.wall) {
          ans += Cell(i, j)
          //println(Cell(i, j))
        }
      }
    }
    ans
  }

  def printHumanly: Unit = {
    for(i <- 0 until mazeProperties.height){
      for(j <- 0 until mazeProperties.width){
        if(matrix(i)(j) == mazeProperties.wall)
          print(1)
        else
          print(0)
      }
      println()
    }

  }

  def Matrix: Array[Array[Int]] = {
    matrix
  }

  def buildWalls: Unit = {
    for(i <- 0 until mazeProperties.width){
      matrix(0)(i) = mazeProperties.wall
      matrix(mazeProperties.height - 1)(i) = mazeProperties.wall
    }

    for(i <- 0 until mazeProperties.height){
      matrix(i)(0) = mazeProperties.wall
      matrix(i)(mazeProperties.width - 1) = mazeProperties.wall
    }
  }

  def runCheck: Int = {
    val matrixCopy = matrix.map(_.clone())
    if (reachability) {
      val sizeI = mazeProperties.height
      val sizeJ = mazeProperties.width

      var j = 1
      var i = 1
      var di = 1
      var dj = 0
      var oneCount = 0
      var stepCount = 0

      while (true) {
        if (matrixCopy(i)(j) == 1) {
          oneCount -= 1
        }
        matrixCopy(i)(j) += 1

        if (matrixCopy(i)(j) == 1) {
          oneCount += 1
        }

        if (oneCount == 0) {
          return -1
        }

        val mi = min(matrixCopy(i + 1)(j), min(matrixCopy(i - 1)(j), min(matrixCopy(i)(j + 1), matrixCopy(i)(j - 1))))
        if (mi == mazeProperties.wall) {
          return -1
        }
        if (matrixCopy(i + di)(j + dj) == mi) {
          // nothing
        }
        else if (matrixCopy(i + 1)(j) == mi) {
          di = 1
          dj = 0
        }
        else if (matrixCopy(i)(j + 1) == mi) {
          di = 0
          dj = 1
        }
        else if (matrixCopy(i - 1)(j) == mi) {
          di = -1
          dj = 0
        }
        else if (matrixCopy(i)(j - 1) == mi) {
          di = 0
          dj = -1
        }
        else {
          println("error by checking")
        }
        i += di
        j += dj
        stepCount += 1
        if (i == sizeI - 2 && j == sizeJ - 2) {
          return stepCount
        }
      }
    }
    else{
      return -1
    }
    0
  }

  def reachability: Boolean = {
    var cellsForCheck = getNeighbors(mazeProperties.sourceCell)
    var ans = false
    breakable {
      while (cellsForCheck.nonEmpty) {
        if (cellsForCheck.contains(mazeProperties.destCell)) {
          ans = true
          break
        }
        val currCell = cellsForCheck(cellsForCheck.size - 1)
        cellsForCheck.remove(cellsForCheck.size -1)
        cellsForCheck.++=(getNeighbors(currCell))
      }
    }
    // reset visited
    visited = Array.fill[Boolean](mazeProperties.width * mazeProperties.height)(false)
    ans
  }

  def getNeighbors(index : Cell): ArrayBuffer[Cell] ={
    visited(index.heightShift * mazeProperties.width + index.widthShift) = true
    val neighbors = new ArrayBuffer[Cell](0)
    if (index.widthShift > 1)
      neighbors += Cell(index.widthShift - 1, index.heightShift)
    if (index.heightShift > 1)
      neighbors += Cell(index.widthShift, index.heightShift - 1)
    if (index.widthShift < mazeProperties.width - 2)
      neighbors += Cell(index.widthShift + 1, index.heightShift)
    if (index.heightShift < mazeProperties.height - 2)
      neighbors += Cell(index.widthShift, index.heightShift + 1)

    var ans = new ArrayBuffer[Cell](0)
    // delete cell-wall
    for(element <- neighbors){
      if((matrix(element.heightShift)(element.widthShift) == 0)
        && (!visited(element.heightShift * mazeProperties.width + element.widthShift))){
        ans += element
      }
    }
    ans
  }

  def inc: Boolean = {
    alg.inc
  }
}

object Maze {
  def apply(): Maze = {
    val ans = new Maze
    ans.buildWalls
    ans
  }
}
