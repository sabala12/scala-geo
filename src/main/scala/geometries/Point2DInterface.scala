package geometries

import scala.language.higherKinds

import scala.math.Ordering
import scala.math.Numeric

import scala.math.Numeric.Implicits._
import scala.Ordering.Implicits._

trait Point2DInterface[T[_]] {
  def Sub[U : Numeric](p1: T[U], p2: T[U]) : Double
  def x[U : Numeric](p: T[U]) : U
  def y[U : Numeric](p: T[U]) : U
  def lowerPoint[U : Numeric](p1: T[U], p2: T[U]): T[U]
  def higherPoint[U : Numeric](p1: T[U], p2: T[U]): T[U]
  def newPoint[U: Numeric](x: U, y: U) : T[U]
  def newPoint[U: Numeric]() : T[U]
  def toDouble2D[U: Numeric](point: T[U]) : T[Double]
}

class ImplPoint2DInterface() extends Point2DInterface[Point2D] {

  def Sub[U : Numeric](p1: Point2D[U], p2: Point2D[U]) = p1 - p2
  def x[U : Numeric](p: Point2D[U]) = p.x
  def y[U : Numeric](p: Point2D[U]) = p.y
  def lowerPoint[U : Numeric](p1: Point2D[U], p2: Point2D[U]) = if (p1.y < p2.y) p1 else p2
  def higherPoint[U : Numeric](p1: Point2D[U], p2: Point2D[U]) = if (p1.y >= p2.y) p1 else p2
  def newPoint[U: Numeric](x: U, y: U) = Point2D(x, y)
  def newPoint[U: Numeric]() = Point2D.apply[U]()
  def toDouble2D[U: Numeric](point: Point2D[U]) = Point2D(point.x.toDouble(), point.y.toDouble())
}

object Point2DImplicits {

  implicit def Point2DInterface: Point2DInterface[Point2D] = {
    new ImplPoint2DInterface()
  }

  implicit class Point2DInterfaceOps[T[_] : Point2DInterface, U : Numeric](point: T[U]) {
    def point2DInterface = implicitly[Point2DInterface[T]]
    def -(other: T[U]) = point2DInterface.Sub(point, other)
    def x: U = point2DInterface.x(point)
    def y: U = point2DInterface.y(point)
    def lowerPoint(other: T[U]) = point2DInterface.lowerPoint(point, other)
    def higherPoint(other: T[U]) = point2DInterface.higherPoint(point, other)
    def newPoint(x: U, y: U): T[U] = point2DInterface.newPoint(x, y)
    def newPoint = point2DInterface.newPoint[U]()
    def toDouble2D() = point2DInterface.toDouble2D(point)
  }
}

//class Point2DInterfaceLowerPointOrdering[T[_] : Point2DInterface, U: Numeric] extends Ordering[T[U]] {
//  import Point2DImplicits.Point2DInterfaceOps
//
//  def compare(p1: T[U], p2: T[U]): Int = (p1.y - p2.y).toInt()
//}
//
//object Point2DInterfaceLowerPointOrdering {
//  def apply[T[_] : Point2DInterface, U: Numeric]() = new Point2DInterfaceLowerPointOrdering[T, U]()
//}

object newInterfacePoint2D {
  def apply[T[_]: Point2DInterface, U: Numeric]() = implicitly[Point2DInterface[T]].newPoint[U]()
  def apply[T[_]: Point2DInterface, U: Numeric](x: U, y: U) = implicitly[Point2DInterface[T]].newPoint(x, y)
}