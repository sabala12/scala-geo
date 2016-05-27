import scala.collection.immutable.TreeMap
import geometries.Triangle2D
import geometries.Point2D
import geometries.Point2D._
import geometries.Point2DInterface
import geometries.Point2DImplicits._

import scala.math.{Ordering, Numeric}


implicit object ordering extends Ordering[Point2D[Double]] {
  def compare(p1: Point2D[Double], p2: Point2D[Double]): Int = {
    if (p1.x != p2.x) (p1.x - p2.x).toInt
    else (p1.y - p2.y).toInt
  }
}

// convert maps to seq, to keep duplicate keys and concat
val a_k1 = Point2D(3.0,3.0)
val a_t1 = Triangle2D(Point2D(2.0,6.0), Point2D(1.0,4.0), Point2D(3.0,3.0))
val a_k2 = Point2D(3.0,1.0)
val a_t2 = Triangle2D(Point2D(1.0,4.0), Point2D(2.0,1.0), Point2D(3.0,3.0))
val a = TreeMap(a_k1 -> List(a_t1), a_k2 -> List(a_t2))
val b_k1 = Point2D(6.0,3.0)
val b_t1 = Triangle2D(Point2D(7.0,9.0), Point2D(5.0,6.0), Point2D(6.0,3.0))
val b_k2 = Point2D(3.0,3.0)
val b_t2 = Triangle2D(Point2D(5.0,6.0), Point2D(5.0,1.0), Point2D(6.0,3.0))
val b_t3 = Triangle2D(Point2D(6.0,3.0), Point2D(5.0,1.0), Point2D(9.0,2.0))
val b = TreeMap(a_k1 -> List(b_t1), a_k2 -> List(b_t2, b_t3))
val r = a ++ b.map({
  case (k,v) => {
    k -> (v ++ a.getOrElse(k, Nil))
  }
})