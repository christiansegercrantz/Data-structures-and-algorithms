// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

import scala.swing._
import java.awt.Color

object visualize extends SimpleSwingApplication {
  val world1 = """
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@SWDWWWDWSWWSWWWWWWWWWWDWW@
@S......D.........D..S...W@
@WSSDDSWWWWWWWWDWWSWWWDW.W@
@W.W.......S...D...DSD.S.W@
@WDS.WWWWSWWWWSWWSWSWW.W.W@
@D.WSW...S......SD.D.WSW.W@
@W.D.W.WWWWWDWWDWWWW.W.W.D@
@D.W.W.D...S......SW.W.W.W@
@W.W.WDW.WWWSWWWWW.W.W.DSW@
@WDW.W.W.W.D*.D..W.W.WDD.W@
@WDW.W.SSWWWWWWWSD.SDW.W.W@
@W.W.S.W...........W.W.W.W@
@W.WDW.WWSWWWWWWWWWW.D.W.S@
@D.W.W.....D.DS.....SW.W.W@
@WSS.DWWDWWWWWWWWWWWWS.W.W@
@W.WS.......S..........W.D@
@W.WSWWWWWWWDWWWWSWWWWWW.W@
@W.D....SS..DDD.S........W@
@WWWWWWWWWWWWWWWWWWWWWWWDW@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
"""
  val world2 = """
@@@@@@@@@@@@@@@@
@WSS..WSWWWWWSS@
@W.WWWWSW..D..S@
@W.DD....*WWW.W@
@SSW.WWWW..W..W@
@W...D..WS.DSDW@
@WWWWWWDWWWW..W@
@@@@@@@@@@@@@@@@
  """

  val world = World(world2)
  println(world)
  // Get the solution path
  val path = solver.solve(world)
  assert(world.isValidEscapePath(path)._1 == true)

  // RGB colors for the different kind of blocks
  val blockColors = Map(Start -> new Color(50, 50, 255),
                        Safe -> new Color(150, 150, 255),
                        Corridor -> new Color(200, 200, 200),
                        Swamp -> new Color(50, 200, 50),
                        Door -> new Color(200, 50, 200),
                        Wall -> new Color(0,0,0))
  val preferredBlockSize = 30
  val rows = world.nofRows
  val columns = world.nofColumns
  private val filled = Array.fill(rows, columns)(false)
  val mazePanel = new Panel {
    background = Color.black
    preferredSize = new Dimension(columns * preferredBlockSize,
                                  rows * preferredBlockSize)

    override protected def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val width = this.size.width
      val height = this.size.height
      //val cellWidth = width.toDouble / columns
      //val cellHeight = height.toDouble / rows

      def xOutLeft(c: Int): Int = ((c * width.toDouble) / columns).toInt
      def xOutRight(c: Int): Int = xOutLeft(c+1)
      val xDelta = 0.15 * width.toDouble / columns
      def xInLeft(c: Int): Int = ((c * width.toDouble) / columns + xDelta).toInt
      def xInRight(c: Int): Int = (((c+1) * width.toDouble) / columns - xDelta).toInt
      val yDelta = 0.15 * height.toDouble / rows
      def yOutTop(r: Int): Int = ((r * height.toDouble) / rows).toInt
      def yOutBot(r: Int): Int = yOutTop(r+1)
      def yInTop(r: Int): Int = ((r * height.toDouble) / rows + yDelta).toInt
      def yInBot(r: Int): Int = (((r+1) * height.toDouble) / rows - yDelta).toInt
      g.setStroke(new java.awt.BasicStroke(0))
      for (r <- 0 until rows) {
        for (c <- 0 until columns) {
          val block = world(r, c)
          if (filled(r)(c)) {
            g.setColor(Color.white)
            g.fillRect(xOutLeft(c), yOutTop(r),
                       xOutRight(c) - xOutLeft(c), yOutBot(r) - yOutTop(r))
            g.setColor(blockColors(block))
            g.fillRect(xInLeft(c), yInTop(r),
                       xInRight(c) - xInLeft(c), yInBot(r) - yInTop(r))
          } else {
            g.setColor(blockColors(block))
            g.fillRect(xOutLeft(c), yOutTop(r),
                       xOutRight(c) - xOutLeft(c), yOutBot(r) - yOutTop(r))
          }
        }
      }
    }
  }
  // A timer for performing the animation
  val timer = new javax.swing.Timer(1000, new javax.swing.AbstractAction() {
    def actionPerformed(e : java.awt.event.ActionEvent) = animate()
  })
  timer.setRepeats(false)

  // Animate the path steps one by one, restart when done
  var animateIndex = -1
  protected def animate(delayInMS: Int = 100): Unit = {
    // Go to the next step
    animateIndex += 1
    // If at the end, reset
    if(animateIndex >= path.length) {
      animateIndex = 0
      for(r <- 0 until rows; c <- 0 until columns)
        filled(r)(c) = false
    }
    // get the currently visited block
    val (r,c) = path(animateIndex)
    val block = world(r, c)
    // Set the animation delay based on the block type
    var delay = delayInMS * block.duration
    // Stop for two extra seconds after the last step before restarting
    if(animateIndex == path.length-1)
      delay += 2000
    // Fill the current block
    filled(r)(c) = true
    mazePanel.repaint()
    // Restart the timer
    timer.setInitialDelay(delay)
    timer.restart()
  }

  // Create the main window
  def top = new MainFrame {
    contents = mazePanel
    // Stop the timer when the window is closed
    reactions += {
      case event.WindowClosing(_) => timer.stop()
    }
    open()
    centerOnScreen()
    timer.start()
  }
}
