package regressiomalli
import javax.swing._
import java.io.File
import java.awt.event.{ActionListener, ActionEvent}

object DataFile {
  // Just contains basic setter and getter for file that contains the data we want to process
  private var _file: Option[File] = None
  
  def setFile(file: File): Unit = {
    _file = Some(file)
  }
  
  def getFile(): Option[File] = {
    _file
  }
}


// Might not be the most elegant solution to the problem of switching and checking states of clicked boxes,
// but since javax swing does not let you use button groups inside a jpanel, it has to be accomplished with "dirty hacks"
object Boxes {
  private var linear:    Option[JRadioButton] = None
  private var parabolic: Option[JRadioButton] = None
  private var scatter:   Option[JCheckBox]    = None

  def setButtons(l: JRadioButton, p: JRadioButton, s: JCheckBox): Unit = {
    linear    = Some(l)
    parabolic = Some(p)
    scatter   = Some(s)
  }
  
  def getScatter(): Boolean = {
    scatter.get.isSelected()
  }
  
  def getLinear(): Boolean = {
    linear.get.isSelected()
  }
  
  def getParabolic(): Boolean = {
    parabolic.get.isSelected()
  }
  
  def getType(): String = {
    if (getLinear) "linear"
    else "parabolic"
  }
  
  def switchRadioButtons(l: Boolean, p: Boolean): Unit = {
    linear.get.setSelected(l)
    parabolic.get.setSelected(p)
  }

}

// Simple wrapper for ActionListeners
// Takes a function as an argument and runs it on certain event
case class Event(private val f: () => Any) extends ActionListener {
  override def actionPerformed(e: ActionEvent): Unit = {
    f()
  }
}

object Events {
  
  def clickRadioButton(l: Boolean, p: Boolean): ActionListener = {
    val func = () => {
      Boxes.switchRadioButtons(l, p)
    }
    Event(func)
  }
  
  // Method that returns a function wrapped inside Event class instance
  def chooseFile(label: JLabel, parent: JFrame, fileChooser: JFileChooser): ActionListener = {
    
    val func = () => {
      val dialog = new JDialog(parent, "File opener")
      val result: Int = fileChooser.showOpenDialog(parent)
    
      if(result == JFileChooser.APPROVE_OPTION) {
        val file: File = fileChooser.getSelectedFile()
        val name = file.getName()
        DataFile.setFile(file)
        val labelText = if (name.size > 15) name.substring(12) + "..." else name
        label.setText(labelText) // Name cant exceed 15 chars
      }
    }
    Event(func)
  }

  
  // Action listener for file submitting
  // So basically, when "Display data" - button is clicked, this goes into action
  // It first checks whether a file has been loaded, and if not, prompts telling so
  // Otherwise, it parses the data and proceeds to continue if 
  def submitFile(label: JLabel, parent: JFrame, button: JButton): ActionListener = {
    val func = () => {
      val file: Option[File] = DataFile.getFile  // will get the current file from helper if such exists
      
      if (file == None) {
        JOptionPane.showMessageDialog(parent, "No file has been loaded")
      } 
      else {
        val data: Array[(Double, Double)] = Parser.parse(file.get)

        if(data.isEmpty) {
          JOptionPane.showMessageDialog(parent, "This file contains no valid data")
        } 
        else {
          val chartPanel = Components.createChart(data, file.get.getName, Boxes.getType, Boxes.getScatter)
          val popup = new JDialog(parent, file.get.getName)
          popup.add(chartPanel)
          popup.setSize(1000, 760)
          popup.setVisible(true)
    
        }
      }
    }
    Event(func)
  }
}