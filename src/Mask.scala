import scala.math._

class Mask(val size: Int, val mask: Int) {
  require(mask < pow(2, size * size), "mask must be less than size squere!")
  require(mask > 0, "mask must be positive only!")

  def toStr: String = {
    val binary = mask.toBinaryString
    "0" * (size * size - binary.length) + binary
  }

  def toMatrix: Array[Array[Char]] = {
    val matrix =  Array.ofDim[Char](size, size)
    for(i <- 0 until  size;
        j <- 0 until  size){
      matrix(i)(j) = toStr(i * size + j)
    }
    matrix
  }

  def next: Mask = new Mask(size, mask + 1)
  def hasNext = mask + 1 < pow(2, size * size)
}
