package geometries

import scala.math._
import scala.Numeric.Implicits._
import scala.language.higherKinds

import Point2DImplicits.Point2DInterfaceOps

class Edge2D[T[_] : Point2DInterface, U : Numeric] (val p1: T[U], val p2: T[U]) {

  def this() {
    this(newInterfacePoint2D(), newInterfacePoint2D())
  }

  def length = p1 - p2

  private lazy val p1_x_double = p1.x.toDouble()
  private lazy val p1_y_double = p1.y.toDouble()
  private lazy val p2_x_double = p2.x.toDouble()
  private lazy val p2_y_double = p2.y.toDouble()

  lazy val b: Double = p1_y_double - (p1_x_double * slope)

  lazy val slope: Double = {
    require(p1.x != p2.x, " now allowed")
    (p1_y_double - p2_y_double) / (p1_x_double - p2_x_double)
  }
}

import Point2DImplicits._

object Edge2D {

  def apply[T[_] : Point2DInterface, U : Numeric]() = new Edge2D[T, U]()
  def apply[T[_] : Point2DInterface, U : Numeric](p1: T[U], p2: T[U]) = new Edge2D(p1, p2)
}