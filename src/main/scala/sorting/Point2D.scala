package sorting

import scala.language.higherKinds
import scala.math.{Ordering, Numeric}
import scala.math.Numeric.Implicits._

import geometries.Point2DInterface
import geometries.Point2DImplicits.Point2DInterfaceOps

object Point2D {

  class High[T[_] : Point2DInterface, U: Numeric] extends Ordering[T[U]] {
    def compare(p1: T[U], p2: T[U]): Int = (p1.y - p2.y).toInt()
  }

  class Low[T[_] : Point2DInterface, U: Numeric] extends Ordering[T[U]] {
    def compare(p1: T[U], p2: T[U]): Int = (p2.y - p1.y).toInt()
  }

  class Right[T[_] : Point2DInterface, U: Numeric] extends Ordering[T[U]] {
    def compare(p1: T[U], p2: T[U]): Int = (p1.x - p2.x).toInt()
  }

  class Left[T[_] : Point2DInterface, U: Numeric] extends Ordering[T[U]] {
    def compare(p1: T[U], p2: T[U]): Int = (p2.x - p1.x).toInt()
  }

  class LowPlusSide[T[_] : Point2DInterface, U: Numeric](horizontalOrdering: Ordering[T[U]]) extends Ordering[T[U]] {
    def compare(p1: T[U], p2: T[U]): Int = {
      val low = new Low[T, U]()
      val r = low.compare(p1, p2)
      if (r == 0) horizontalOrdering.compare(p1, p2)
      else r
    }
  }
}