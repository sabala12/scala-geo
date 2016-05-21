package geometries

import scala.math._
import scala.Numeric.Implicits._
import scala.language.higherKinds

import Point2DImplicits.Point2DInterfaceOps

class Circle2D[T[_] : Point2DInterface, U : Numeric] (val center: T[U], val radius: U) {

  private lazy val center_x_double = center.x.toDouble()
  private lazy val center_y_double = center.y.toDouble()
  private lazy val radius_double = radius.toDouble()

  def isInside(point: T[U]) = {
    val dx = pow(point.x.toDouble() - center_x_double, 2)
    val dy = pow(point.y.toDouble() - center_y_double, 2)
    sqrt(dx + dy) < radius_double
  }
}

object Circle2D {

  private def from3Points[T[_]: Point2DInterface, U: Numeric](p1: T[U], p2: T[U], p3: T[U]): (T[U], U) = {
    val m1 = Edge2D(p1, p2).slope
    val m2 = Edge2D(p2, p3).slope
    val x1 = p1.x.toDouble()
    val x2 = p2.x.toDouble()
    val x3 = p3.x.toDouble()
    val y1 = p1.y.toDouble()
    val y2 = p2.y.toDouble()
    val y3 = p3.y.toDouble()
    val x_center = (m1 * m2 * (y3 - y1) + m1 * (x2 + x3) - m2 * (x1 + x2)) / (2 * (m1 - m2))
    // TODO newInterfacePoint2D.apply[T, Double]() without 'apply[T, Double]'
    val (slope_z, median_z) = if (m1 == 0) (m2, newInterfacePoint2D((x2 + x3) / 2.0, (y2 + y3) / 2.0))
                              else (m1, newInterfacePoint2D((x1 + x2) / 2.0, (y1 + y2) / 2.0))
    if (slope_z == 0) throw new Exception("Three horizontal points cannot construct a circle! points=" + p1 + "|" + p2 + "|" + p3)

    val perpemdicularLine = Line2D(median_z, -1.0 / slope_z)
    val y_center = perpemdicularLine.getY(x_center)
    val center = newInterfacePoint2D(x_center, y_center)
    val radius = center - p1.toDouble2D()
    (center.asInstanceOf[T[U]], radius.asInstanceOf[U])
  }

  def apply[T[_]: Point2DInterface, U: Numeric](p1: T[U], p2: T[U], p3: T[U]) = {
    val (center, radius) = from3Points(p1, p2, p3)
    new Circle2D(center, radius)
  }
}