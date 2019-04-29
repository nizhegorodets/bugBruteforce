object mazeProperties {
  def width = 31
  def height = 21
  def wall = Int.MaxValue
  def notWall = 0
  def sourceCell = Cell(1,1)
  def destCell = Cell(width - 2, height - 2)
  def cellNumbers = (width - 2)*(height - 2)
}
