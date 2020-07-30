package regressiomalli
import javax.swing._
import java.awt.{ Color, Shape, BasicStroke, BorderLayout}
import java.io.File
import java.awt.geom.Ellipse2D
import org.jfree.chart._
import org.jfree.data.xy._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.{XYLineAndShapeRenderer, XYDotRenderer}

object Components {
  // Parent 
  private var _parent: Option[JFrame] = None
  
  def getParent(): JFrame = {
    _parent.get
  }
  
  def setParent(parent: JFrame): Unit = {
    _parent = Some(parent)
  }
  
  // Adds all the basic components needed for that starting menu
  def addBasicComponents(): JComponent = {
    val mainPanel:  JComponent = getMainPanel()
    val fileOpener: JComponent = getFileOpener()
    mainPanel.add(fileOpener)
    mainPanel
  }
  
  // Creates a mainpanel which will contain all the other stuff
  def getMainPanel(): JComponent = {
    val mp = new JPanel()
    mp.setName("mainpanel")    // Give it a name so it can be referred later on
    mp
  }
  
  def getCheckBoxes(): JComponent = {
    val panel = new JPanel()
    
    val l = new JRadioButton("Linear")
    val p = new JRadioButton("Parabolic")
    val s = new JCheckBox("Scatter")
    
    l.setSelected(true); p.setSelected(false); s.setSelected(true)
    
    l.addActionListener(Events.clickRadioButton(true, false))
    p.addActionListener(Events.clickRadioButton(false, true))
    
    Boxes.setButtons(l, p, s)
    
    panel.add(l); panel.add(p); panel.add(s)
    panel
  }
  
  // Creates all the components that are needed for filechooser to operate
  def getFileOpener(): JComponent = {
    val outerPanel = new JPanel()
    // Using boxlayout lets me have content on two separate rows
    outerPanel.setLayout(new BoxLayout(outerPanel, BoxLayout.PAGE_AXIS))
    
    val panel = new JPanel()
    val label = new JLabel("Filename")
    val btn1  = new JButton("Choose File")
    val btn2  = new JButton("Display Data")
    val fc    = new JFileChooser()
   
    fc.setFileFilter(new filechooser.FileNameExtensionFilter("Text", "txt"))
    
    btn1.addActionListener(Events.chooseFile(label, getParent, fc))
    btn2.addActionListener(Events.submitFile(label, getParent, btn2))
    panel.add(label)
    panel.add(btn1)
    panel.add(btn2)
    outerPanel.add(panel)
    outerPanel.add(getCheckBoxes)
    outerPanel
  }
  
  
  // Turns array with tuples into XYseries
  def createSeries(arr: Array[(Double, Double)], name: String): XYSeries = {
    val series = new XYSeries(name)
    for (a <- arr) {
      series.add(a._1, a._2)
    }
    series
  }
  
  // Creates chart from given dataset
  def createChart(arr: Array[(Double, Double)], name: String, lineType: String, scatter: Boolean): ChartPanel = {
    val dataset = new XYSeriesCollection()  // Init new dataset
    
    val regressionFunction = Regression.createRegression(arr, lineType)
    
    val dataArray: Array[(Double, Double)] = regressionFunction._1
    val series = createSeries(dataArray, regressionFunction._2)
    
    dataset.addSeries(series)
    
    if (scatter) {
      val points = createSeries(arr, "Scatter data")
      dataset.addSeries(points)
    }
    
    val chart = ChartFactory.createXYLineChart(
        name,
        "X", "Y",
        dataset,
        PlotOrientation.VERTICAL,
        true, true, false
    )
    
    val plot = chart.getXYPlot()
    val renderer = new XYLineAndShapeRenderer()
    
    renderer.setSeriesPaint(0, Color.BLUE)
    renderer.setSeriesShape(0, new Ellipse2D.Double(0, 0, 1, 1))
    //renderer.setSeriesStroke(0, new BasicStroke(1.0f));
    
    if (scatter) {
      renderer.setSeriesPaint(1, Color.RED)
      renderer.setSeriesLinesVisible(1, false)
      renderer.setSeriesShape(1, new Ellipse2D.Double(0, 0, 2, 2))
    }
    
    plot.setRenderer(renderer)
    plot.setBackgroundPaint(Color.WHITE)
    
    val cp = new ChartPanel(chart)
    cp
  }
}