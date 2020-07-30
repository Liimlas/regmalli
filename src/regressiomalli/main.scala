package regressiomalli
import javax.swing._
import java.awt.Color

class Regressiomalli extends JFrame with Runnable {
  private val screenSize   = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  private val screenWidth  = screenSize.getWidth()
  private val screenHeight = screenSize.getHeight()

  // Trying to keep main file as minimal and clean as possible, so a lot
  // of functionality has been moved elsewhere
  def run(): Unit = {
    this.setMaximumSize(screenSize)             // Size of to window can exceed the height and width of one's screen
    this.setTitle("Regressiomalli")
    this.setSize(400, 300)                      // This is just random size that I chose
    this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)  // 
    Components.setParent(this)                  // Give reference to Components-object so it can be used to create additional components
    this.add(Components.addBasicComponents())
    this.setVisible(true)                       // JFrame is hidden by default, so it needs to be set visible
  }
}


// Function that starts the whole program
object Regressiomalli {
  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(new Regressiomalli())
  }
}