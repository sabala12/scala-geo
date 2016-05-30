package geometries

import scala.Numeric.Implicits._
import scala.math._

class Point2D[U : Numeric] (val x: U, val y: U) {

  def numeric = implicitly[Numeric[U]]

  def -(other: Point2D[U]): Double = {
    val dx = pow(x.toDouble() - other.x.toDouble(), 2)
    val dy = pow(y.toDouble() - other.y.toDouble(), 2)
    sqrt(dx + dy)
  }

  def direction(other: Point2D[U]): Double = {
    atan2(
      other.y.toDouble() - y.toDouble(),
      other.x.toDouble() - x.toDouble()
    )
  }

  lazy val toInt: Point2D[Int] = {
    new Point2D(numeric.toInt(x), numeric.toInt(y))
  }

  lazy val toDouble: Point2D[Double] = {
    new Point2D(numeric.toDouble(x), numeric.toDouble(y))
  }

  lazy val toFloat: Point2D[Float] = {
    new Point2D(numeric.toFloat(x), numeric.toFloat(y))
  }

  override lazy val toString: String = x.toString + "," + y.toString

  override def equals(obj: Any): Boolean = obj match {
    case that: Point2D[_] => that.x.equals(x) && that.y.equals(y)
    case _ => false
  }

  override def hashCode(): Int = toString.hashCode
}

object Point2D {

  def apply[U : Numeric]() = {
    val zero: U = implicitly[Numeric[U]].fromInt(0)
    new Point2D(zero, zero)
  }
  def apply[U : Numeric](x: U, y: U) = new Point2D(x, y)
}