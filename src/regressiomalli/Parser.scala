package regressiomalli
import java.io.{File}
import scala.io.Source
import scala.collection.mutable.Buffer

object Parser {

  // Function for parsing data out of text format
  def parseText(points: Array[String]): Array[(Double, Double)] = {
    val data = Buffer[(Double, Double)]()
    
    for(point <- points) {
      try {
        val p = point.split(",")
        data.append((p(0).toDouble, p(1).toDouble))
      }
      catch {
        // If the point is somewhat invalid, it just prints to the console and skips it
        case _ : Throwable => println(s"Invalid point ${point}")
      }
    }
    data.toArray
  }
  
  def parse(file: File): Array[(Double, Double)] = {
    val name: String = file.getName()
    val dotIndex: Int = name.lastIndexOf('.')
    val extension: String = name.substring(dotIndex + 1)
    extension match {
      case "txt" => {
        val points: Array[String] = Source.fromFile(file, "ISO-8859-1").getLines().mkString.split(";")
        val data: Array[(Double, Double)] = parseText(points)
        data
      }
      // Incase of a unknown file extension or problem situation, we just return an empty array
      // The point of doing this, is that it helps dealing with problems
      case _     => {
        Array[(Double, Double)]()
      }
    }
  }
}