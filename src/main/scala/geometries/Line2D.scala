package geometries

import scala.math._
import scala.Numeric.Implicits._
import scala.language.higherKinds

import Point2DImplicits.Point2DInterfaceOps

class Line2D[T[_] : Point2DInterface, U : Numeric] (val point: T[U], val slope: Double) {
  private lazy val d: Double = point.y.toDouble() - (slope * point.x.toDouble())
  def getY(x: U): Double = d + slope*x.toDouble()
  def getX(y: U): Double = (y.toDouble() - d) / slope
}

object Line2D {
  def apply[T[_] : Point2DInterface, U : Numeric](p1: T[U], p2: Double) = new Line2D(p1, p2)
}
