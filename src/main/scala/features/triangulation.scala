package features

import com.sun.javafx.geom.Edge
import sorting.Point2D.Right

import scala.math._
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.language.higherKinds

import scala.collection.immutable.TreeMap

import geometries._
import geometries.Point2DImplicits.Point2DInterfaceOps

object triangulation {

  type TrianglesMap[T[_], U] = TreeMap[T[U], Seq[Triangle2D[T, U]]]
  type PointsSeq[T[_], U] = Seq[T[U]]
  type PointsSeqContainer[T[_], U] = Seq[Seq[T[U]]]

  def apply[T[_] : Point2DInterface, U : Numeric](points: PointsSeq[T, U]): TrianglesMap[T, U] = {

    type Point = T[U]
    type Triangle = Triangle2D[T, U]
    type PointsSet = Set[Point]
    type PointsSeq = Seq[Point]
    type PointsDoubleSeq = Seq[Seq[Point]]
    type TrianglesMap = TreeMap[Point, Seq[Triangle]]
    object TrianglesMap {
      def apply() = new TrianglesMap()(new sorting.Point2D.Order())
    }

    def TrianglesMapWithValue(point: Point, trianglesSeq: Seq[Triangle]) = {
      TrianglesMap().insert(point, trianglesSeq)
    }

    def pointsSeqBase(pointsSeq: PointsSeq) = {
      if (pointsSeq.size > 1) (pointsSeq.head, pointsSeq.tail)
      else if (pointsSeq.nonEmpty) (pointsSeq.head, List[Point]())
      else throw new Exception("Empty PointsSeq!")
    }

    def divideToSubsets(points: PointsSeq, n: Int): PointsDoubleSeq = {
      if (points.length <= n) Seq(points)
      else {
        val (leftSide, rightSide) = points.splitAt(points.length / 2)
        val ls = divideToSubsets(leftSide, n)
        val rs = divideToSubsets(rightSide, n)
        ls ++ rs
      }
    }

    def triangulate(points: PointsSeq): TrianglesMap = {
      val sortedPoints = points.sortWith((p1, p2) => {
        if (p1.x != p2.x) p1.x < p2.x
        else p1.y < p2.y
      })

      val partitions = divideToSubsets(sortedPoints, 3)
      val (left, right) = partitions.splitAt(partitions.length / 2)
      merge(left, right)
    }

    def merge(left: PointsDoubleSeq, right: PointsDoubleSeq): TrianglesMap = {

      def setsRecursive(leftPoints: PointsDoubleSeq, rightPoints: PointsDoubleSeq): TrianglesMap = {
        def split(points: PointsDoubleSeq): TrianglesMap = {
          val (leftSide, rightSide) = points.splitAt(points.length / 2)
          merge(leftSide, rightSide)
        }

        val leftTriangles = split(leftPoints)
        val rightTriangles = split(rightPoints)
        val newTriangles = mergeTrees(leftTriangles, rightTriangles)
        val leftAndRightTriangles = combineTrees(leftTriangles, rightTriangles)
        combineTrees(leftAndRightTriangles, newTriangles)
      }

      def subSetsRecursive(leftPoints: PointsSeq, rightPoints: PointsSeq): TrianglesMap = {
        val sort = (p1: Point, p2: Point) => {
          if (p1.y != p2.y) p1.y < p2.y
          else p1.x > p2.x
        }
        val leftSet = leftPoints.sortWith(sort)
        val rightSet = rightPoints.sortWith(sort)
        mergeSegments(leftSet, rightSet)
      }

      (left, right) match {
        case (leftHead::Nil, rightHead::Nil) => subSetsRecursive(leftHead, rightHead)
        case (leftHead::leftRest, rightHead::rightRest) => setsRecursive(left, right)
        case (_, _) => TrianglesMap()
      }
    }

    def mergeSegments(left: PointsSeq, right: PointsSeq): TrianglesMap = {

      def findCandidate(base: Point, otherBase: Point, candidates: PointsSeq): Option[Point] = {
        //TODO calc min angle: Ordering
        for (candidate <- candidates) {
          val circle = Circle2D(candidate, base, otherBase)
          if (candidates.forall(!circle.isInside(_))) return Some(candidate)
        }
        None
      }
      val leftBase = left.head
      val rightBase = right.head
      val leftCandidate = findCandidate(leftBase, rightBase, left.tail)
      val rightCandidate = findCandidate(rightBase, leftBase, right.tail)
      val lowerBase = leftBase lowerPoint rightBase

      def recursiveCall(first: PointsSeq, second: PointsSeq, candidate: Point): TrianglesMap = {
        val newTriangle = Triangle2D(leftBase, rightBase, candidate)
        val newTriangleMap = TrianglesMapWithValue(lowerBase, Seq(newTriangle))
        combineTrees(mergeSegments(first.tail, second), newTriangleMap)
      }

      (leftCandidate, rightCandidate) match {
        case (Some(lc), None) => recursiveCall(left, right, lc)
        case (None, Some(rc)) => recursiveCall(right, left, rc)
        case (Some(lc), Some(rc)) => {
          val leftCircle = Circle2D(leftBase, rightBase, lc)
          if (leftCircle.isInside(rc)) recursiveCall(right, left, lc)
          else recursiveCall(right, left, rc)
        }
        case (None, None) => {
          TrianglesMap()
        }
      }
    }

    def mergeTrees(left: TrianglesMap, right: TrianglesMap): TrianglesMap = {

      val leftKey = left.head._1
      val rightKey = right.head._1

      val l = recalculateTree(left, right.head._1)
      val r = recalculateTree(right, left.head._1)

      def recursiveCall(first: TrianglesMap, second: TrianglesMap): TrianglesMap = {
        combineTrees(mergeTrees(first.tail, second), first)
      }

      (l, r) match {
        case (Some((lc, lt)), None) => {
          recursiveCall(lt, right)
        }
        case (None, Some((rc, rt))) => {
          recursiveCall(rt, left)
        }
        case (Some((lc, lt)), Some((rc, rt))) => {
          val leftCircle = Circle2D(leftKey, rightKey, lc)
          if (leftCircle.isInside(rc)) recursiveCall(lt, right)
          else recursiveCall(rt, left)
        }
        case (None, None) => {
          TrianglesMap()
        }
      }
    }

    def recalculateTree(first: TrianglesMap, otherBase: Point): Option[(Point, TrianglesMap)] = {

      def getCandidate(triangle: Triangle, base: Point, nextBase: Point) = {
        triangle.points
          .filter(_ != base)
          //TODO calc min angle: Ordering
        triangle.points.head
      }

      var triangles = first.head._2
      if (triangles.isEmpty) return None

      val base = first.head._1

      val candidates = triangles.map(getCandidate(_, base, otherBase))
      for (candidate <- candidates) {
        val circle = Circle2D(candidate, base, otherBase)
        if (!candidates.forall(!circle.isInside(_))) triangles = triangles.tail
        else return Some(candidate, first.tail.insert(base, triangles))
      }
      None
    }

    def combineTrees(first: TrianglesMap, second: TrianglesMap): TrianglesMap = {
      first ++ second.map({
        case (k,v) => {
          k -> (v ++ first.getOrElse(k, Nil))
        }
      })
    }

    triangulate(points)
  }
}