package geometries

import scala.math._
import scala.Numeric.Implicits._
import scala.language.higherKinds

import scala.collection.immutable.TreeMap

import Point2DImplicits.Point2DInterfaceOps

class Polygon2D[T[_] : Point2DInterface, U : Numeric](val points: Seq[T[U]]) {

  require(points.size > 2, " A polygon needs 3 or more points, you gave " + points.size)

  def apply(i: Int): T[U] = {
    require(i < points.size, " Index cannot exceed polygon range " + (points.length - 1))
    require(i >= 0, " Negative index, i= " + i)
    points(i)
  }

  lazy val _area: Double = {
    import features.area
    area(this)
  }

  lazy val triangulate: TreeMap[T[U], Seq[Triangle2D[T, U]]] = {
    import features.triangulation
    triangulation(points)
  }

}

import Point2DImplicits._

object Polygon2D {

  def apply[T: Numeric](points: Point2D[T]*) = new Polygon2D(points)
}