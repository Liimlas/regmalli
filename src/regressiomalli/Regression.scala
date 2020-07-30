package regressiomalli
import scala.math._


// Regression is an object used mainly for calculating regression models and for helper functions 
object Regression {
  
  
  def createRegression(arr: Array[(Double, Double)], lineType: String): (Array[(Double, Double)], String) = {
    if(lineType == "linear") {
      createLinearRegression(arr)
    }
    else {
      createParabolicRegression(arr)
    }
  }
  
  // This might seem messy, but essentially it's just calculating means, and values that are needed for the modeling
  // Returns array containing two (x, y) points. First point has value of x_min and y calculated for it
  // Then there is of course x_max, and y for it
  // The regression line is drawn between those two points
  
  def createLinearRegression(arr: Array[(Double, Double)]): (Array[(Double, Double)], String) = {
    val len = arr.size.toDouble      // Casting to double, because dividing by integer might yield a rounded the result
    assert(len >= 1, "The given array was empty") // Basically this should always end up being true, but weird stuff can always happen
    
    val xs: Array[Double] = arr.map(tuple => tuple._1) // Will only contain x's from the original array
    val ys: Array[Double] = arr.map(tuple => tuple._2) // Will only contain y's from the original array
    
    val xMean: Double = xs.sum / len // Calculate the mean of x's
    val yMean: Double = ys.sum / len // Calculate the mean of y's
    
    val xy:    Double = arr.map { case (x,y) => x * y }.sum / len // Calculates the average product of x and y
    
    val xPow:  Double = xs.map(x => math.pow(x, 2)).sum / len
    
    val slope: Double = (xMean * yMean - xy) / (math.pow(xMean, 2) - xPow) // Calculates the slope of the function
    val b:     Double = yMean - slope * xMean // Calculates the constant for the function
        
    val f = (x: Double) => slope * x + b
    
    val x_min: Double = xs.min
    val x_max: Double = xs.max
    
    val str = funcToStr(b, slope)
    
    // Returns array containing the points that regression line is drawn from
    (Array[(Double, Double)]((x_min, f(x_min)), (x_max, f(x_max))), str)
  }
  
  
  def createParabolicRegression(arr: Array[(Double, Double)]): (Array[(Double, Double)], String) = {
    val len: Double = arr.size.toDouble
    assert(len >= 3, "The given array does not contain enough data to form quadratic regression formula")
    val xs: Array[Double] = arr.map(tuple => tuple._1)
    val ys: Array[Double] = arr.map(tuple => tuple._2)
    
    val xMin: Double = xs.min
    val xMax: Double = xs.max
    val step: Double = (xMax - xMin) / 200
    
    
    // Arrays that work as a 3 x 3 matrix, and a 1 x 3 matrix
    val M: Array[Double] = Array.ofDim(9)
    val b: Array[Double] = Array.ofDim(3)
    
    
    // Assign values for each element of matrix
    M(0) = len
    M(1) = xs.sum
    M(3) = M(1)
    M(2) = xs.map(x => pow(x, 2)).sum
    M(4) = M(2)
    M(6) = M(2)
    M(5) = xs.map(x => pow(x, 3)).sum
    M(7) = M(5)
    M(8) = xs.map(x => pow(x, 4)).sum
    
    b(0) = ys.sum
    b(1) = arr.map { case(x, y) => x * y }.sum
    b(2) = arr.map { case(x, y) => pow(x, 2) * y }.sum
    
    val detM:  Double = determinant(M)
    val detM0: Double = determinant(replaceColumn(M.clone, b.clone, 0))
    val detM1: Double = determinant(replaceColumn(M.clone, b.clone, 1))
    val detM2: Double = determinant(replaceColumn(M.clone, b.clone, 2))
    
    val a0: Double = detM0 / detM
    val a1: Double = detM1 / detM
    val a2: Double = detM2 / detM
    
    val function = (x: Double) => a2 * pow(x, 2) + a1 * x + a0
    
    val res: Array[(Double, Double)] = Array.ofDim(200)
    
    for (i <- 0 until 200) {
      val x = xMin + i * step
      val y = function(x)
      res(i) = (x, y)
    }
    val str = funcToStr(a0, a1, a2)
    (res, str)
  }
  
  
  // Helper function for replacing column of matrix
  def replaceColumn(arr: Array[Double], col: Array[Double], i: Int): Array[Double] = {
    arr(i)     = col(0)
    arr(3 + i) = col(1)
    arr(6 + i) = col(2)
    arr
  }
  
  // Calculates the determinant of a 3*3 matrix
  // This is used for getting the parabolic regression
  def determinant(arr: Array[Double]): Double = {
    assert(arr.size == 9)
    val t1: Double = arr(0) * (arr(4) * arr(8) - arr(5) * arr(7))
    val t2: Double = arr(1) * (arr(3) * arr(8) - arr(5) * arr(6))
    val t3: Double = arr(2) * (arr(3) * arr(7) - arr(4) * arr(6))
    t1 - t2 + t3
  }
  
  
  // Returns function in a string format. If a2, the factor of second degree variable is zero, a representation of linear function will be returned
  def funcToStr(a0: Double, a1: Double, a2: Double = 0): String = {
    val form = (n: Double) => if(n > 0) s"+ ${n}" else if(n == 0) "" else s"- ${n * (-1)}" // Used to for formatting - and + signs correctly
    val dec  = (n: Double) => (n * 100000).toInt / 100000.0 // Rounds the number up to 6 digits
    
    if (a2 == 0) {
      "y = " + s"${dec(a1)}*x " + form(dec(a0))
    }
    else {
      "y = " + s"${dec(a2)}*x^2 " + s"${form(dec(a1))}*x " + s"${form(dec(a0))}"
    }
  }
  
}