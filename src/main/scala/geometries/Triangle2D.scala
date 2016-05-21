package geometries

import scala.math._
import scala.language.higherKinds

class Triangle2D[T[_] : Point2DInterface, U : Numeric] private (val points: Seq[T[U]]) {

  def this(p1: T[U], p2: T[U], p3: T[U]) {
    this(Seq(p1,p2,p3))
  }

  def apply(i: Int): T[U] = {
    require(i <= 2, " Index cannot exceed triangle range. range=" + 3)
    require(i >= 0, " Negative index, i= " + i)
    points(i)
  }

  def reduceLeft(op: (T[U], T[U]) => T[U]): T[U] = {
    points.reduceLeft(op)
  }

  def bla() = 3
}

object Triangle2D {

  def apply[T[_] : Point2DInterface, U : Numeric](p1: T[U], p2: T[U], p3: T[U]) = new Triangle2D(p1, p2, p3)
}

//class TrianglePolygon2D(val points: Seq[DoublePoint2D]) extends Triangle2D[DoublePoint2D] {
//  require(points.size == 3, " An triangle needs exactly 3 points, you gave " + points.size)
//
//  lazy val area: Double = {
//    val baseEdge = Edge2D(points(0), points(1))
//
//    distance(baseEdge, points(2)) * baseEdge.length * 0.5
//  }
//}