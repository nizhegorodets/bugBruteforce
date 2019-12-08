class maskAlg(mz: Maze) extends Algorithm {
  var msk = new Mask(4, 1)
  var startCell = Cell(1, 1)
  var record = 0
  var copy = Array.ofDim[Int](msk.size, msk.size)

  override def inc: Boolean = {
    var ans = true
    while(msk.hasNext){
      applyMask(startCell, msk)
      if (mz.reachability){
        if (mz.runCheck > record){
          record = mz.runCheck
          msk = new Mask(msk.size, 1)
          startCell = Cell(1, 1)
          mz.printToFile(record.toString)
          println(record)
        }
        else
          rollbackMask(startCell, msk)
      }
      else
        rollbackMask(startCell, msk)
      msk = msk.next
      //println(msk.mask)
    }
    val y = startCell.widthShift
    val x = startCell.heightShift + 1

    if (x > mazeProperties.width - msk.size - 1){
      startCell = Cell(y + 1, 1)
    }
    else {
      startCell = Cell(y, x)
    }

    if (y > mazeProperties.height - msk.size - 1) ans = false

    msk = new Mask(msk.size, 1)
    ans
  }

  def rollbackMask(startCell: Cell,  msk: Mask): Unit = {
    for(i <- 0 until msk.size;
        j <- 0 until  msk.size){
      mz.Matrix(startCell.widthShift + i)(startCell.heightShift + j) = copy(i)(j)
    }
  }

  def applyMask(startCell: Cell, msk: Mask): Unit =  {

    for(i <- 0 until msk.size;
        j <- 0 until  msk.size){
        // clear (delete this) and copy
        copy(i)(j) = mz.Matrix(startCell.widthShift + i)(startCell.heightShift + j)
        mz.Matrix(startCell.widthShift + i)(startCell.heightShift + j) = mazeProperties.notWall


        val value = if (msk.toMatrix(i)(j) == '0') mazeProperties.notWall else mazeProperties.wall
        mz.Matrix(startCell.widthShift + i)(startCell.heightShift + j) = value


    }
    //mz.printToFile(record.toString)
  }
}
