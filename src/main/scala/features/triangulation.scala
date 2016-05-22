package features

import sorting.Point2D.Right

import scala.math._
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.language.higherKinds

import scala.collection.immutable.TreeMap

import geometries.Triangle2D
import geometries.Circle2D
import geometries.Point2DInterface
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
      def apply() = new TrianglesMap()(new sorting.Point2D.Low())
    }

    def TrianglesMapWithValue(point: Point, trianglesSeq: Seq[Triangle]) = {
      TrianglesMap().insert(point, trianglesSeq)
    }

    def trianglesMapBase(trianglesMap: TrianglesMap) = trianglesMap.head._1
    def trianglesMapCandidates(trianglesMap: TrianglesMap) = trianglesMap.values.head.flatMap(_.points)

    def pointsSeqBase(pointsSeq: PointsSeq) = pointsSeq.head
    def pointsSeqCandidates(pointsSeq: PointsSeq) = pointsSeq.tail

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

      def TrianglesRecursive(leftPoints: PointsDoubleSeq, rightPoints: PointsDoubleSeq): TrianglesMap = {
        def split(points: PointsDoubleSeq): TrianglesMap = {
          val (leftSide, rightSide) = points.splitAt(points.length / 2)
          merge(leftSide, rightSide)
        }
        val leftTriangles = split(leftPoints)
        val rightTriangles = split(rightPoints)
        val newTriangles = mergeContainers(leftTriangles, rightTriangles)(trianglesMapBase, trianglesMapCandidates)
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
        mergeContainers(leftSet, rightSet)(pointsSeqBase, pointsSeqCandidates)
      }

      (left, right) match {
        case (leftHead::Nil, rightHead::Nil) => subSetsRecursive(leftHead, rightHead)
        case (leftHead::leftRest, rightHead::rightRest) => TrianglesRecursive(left, right)
        case (_, _) => TrianglesMap()
      }
    }

    def mergeContainers[Container <: Iterable[_]](left: Container, right: Container)
                                         (getBase: Function[Container, Point],
                                          getCandidates: Function1[Container, PointsSeq]): TrianglesMap = {

      val leftBasePoint = getBase(left)
      val rightBasePoint = getBase(right)
      val leftCandidates = getCandidates(left)
      val rightCandidates = getCandidates(right)

      //def leftPoint(p1: Point, p2: Point) = p1.x < p2.x
      //def rightPoint(p1: Point, p2: Point) = p1.x > p2.x

      import sorting.Point2D.Left
      import sorting.Point2D.Right

      val leftCandidate = selectCandidate(leftCandidates, leftBasePoint, rightBasePoint)(new Right[T,U]())
      val rightCandidate = selectCandidate(rightCandidates, leftBasePoint, rightBasePoint)(new Left[T,U]())

      val lowerBase = leftBasePoint lowerPoint rightBasePoint

      def recursiveCall(newPoint: Point, subLeft: Container, subRight: Container): TrianglesMap = {
        val newTriangle = Triangle2D(newPoint, leftBasePoint, rightBasePoint)
        val trianglesMap = TrianglesMapWithValue(lowerBase, Seq(newTriangle))
        combineTrees(trianglesMap, mergeContainers(subLeft, subRight)(getBase, getCandidates))
      }

      def tailAsContainer(container: Container) = container.tail.asInstanceOf[Container]
      (leftCandidate, rightCandidate) match {
        case (Some(lc), None) => {
          recursiveCall(lc, tailAsContainer(left), right)
        }
        case (None, Some(rc)) => {
          recursiveCall(rc, left, tailAsContainer(right))
        }
        case (Some(lc), Some(rc)) => {
          val leftCircle = Circle2D(lc, leftBasePoint, rightBasePoint)
          if (leftCircle.isInside(rc)) recursiveCall(rc, left, tailAsContainer(right))
          else recursiveCall(lc, tailAsContainer(left), right)
        }
        case (None, None) => {
          TrianglesMap()
        }
      }
    }

    def combineTrees(first: TrianglesMap, second: TrianglesMap): TrianglesMap = {
      val merged = first.toSeq ++ second.toSeq
      val grouped = merged.groupBy(_._1)
      val cleaned = grouped.mapValues(v1 => v1.flatMap(v2 => v2._2).distinct)
      val r = TrianglesMap() ++ cleaned
      r
    }

    def selectCandidate(candidates: PointsSeq, basePoint: Point, otherBasePoint: Point)(ordering: Ordering[Point]) : Option[Point] = {

      import scala.collection.immutable.TreeSet
      val sortedCandidates = TreeSet(candidates: _*)(ordering)
      sortedCandidates.find(candidate => {
        if (candidate == basePoint || candidate == otherBasePoint) false
        else {
          val circle = Circle2D(candidate, basePoint, otherBasePoint)
          sortedCandidates.forall(point => {
            if (candidate == point) true
            else !circle.isInside(point)
          })
        }
      })
    }

    triangulate(points)
  }
}
