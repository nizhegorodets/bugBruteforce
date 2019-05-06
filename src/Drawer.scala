import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._

class Drawer(maze: Maze, random: Int, path: String) {
  def draw: Unit = {
    // Size of image
    val size = (mazeProperties.width * 20, mazeProperties.height * 20 + 50)

    // create an image
    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

    // get Graphics2D for the image
    val g = canvas.createGraphics()

    // clear background
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

    // enable anti-aliased rendering (prettier lines and circles)
    // Comment it out to see what this does!
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)


    // draw a filled and an unfilled Rectangle
    g.setColor(Color.BLACK)
    for(i <- 0 until mazeProperties.height){
      for(j <- 0 until mazeProperties.width){
        if(maze.Matrix(i)(j) == mazeProperties.wall){
          g.fill(new Rectangle2D.Double(j * 20, i * 20, 20, 20))
        }
      }
    }

    // draw some text
    g.setColor(new Color(0, 128, 0)) // a darker green
    g.setFont(new Font("Batang", Font.PLAIN, 20))
    val steps = maze.runCheck
    val numberOfDigits = steps.toString.length
    g.drawString(s"$steps", mazeProperties.width * 20 / 2 - 20 * (numberOfDigits / 2), mazeProperties.height * 20 + 35)

    // done with drawing
    g.dispose()

    // write image to a file
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File(s"$path.png"))
  }
}
