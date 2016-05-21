package features

import geometries._

import scala.math._
import scala.Numeric.Implicits._
import scala.language.higherKinds
import Point2DImplicits.Point2DInterfaceOps

object area {

  def apply[T[_] : Point2DInterface, U : Numeric](triangle: Triangle2D[T, U]): Double = {
    0
  }

//  def apply[T : Point2DInterface, U : Numeric](rectangle: Rectangle2D[T, U]): Double = {
//    0
//  }

  def apply[T[_] : Point2DInterface, U : Numeric](polygon: Polygon2D[T, U]): Double = {
    val points = polygon.points
    val n = points.size
    val i1 = 0 until n
    val i2 = (1 until n) :+ 0
    var p1 = 0D
    var p2 = 0D
    for (i <- 0 until n) {
      p1 = p1 + points(i1(i)).x.toDouble * points(i2(i)).y.toDouble
      p2 = p2 + points(i2(i)).x.toDouble * points(i1(i)).y.toDouble
    }
    abs((p1 - p2) / 2D)
  }
}