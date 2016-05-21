//import scala.collection.immutable.TreeMap
//// convert maps to seq, to keep duplicate keys and concat
//val a = TreeMap(1 -> List(2,4), 2 -> List(0,10))
//val b = TreeMap(1 -> List(2,5), 2 -> List(1,3))
//
////val merged = a.toSeq ++ b.toSeq
//val merged = a.toSeq ++ b.toSeq
//// merged: Seq[(Int, Int)] = ArrayBuffer((1,2), (1,4))
//
//// group by key
//val grouped = merged.groupBy(_._1)
//// grouped: scala.collection.immutable.Map[Int,Seq[(Int, Int)]] = Map(1 -> ArrayBuffer((1,2), (1,4)))
//
//// remove key from value set and convert to list
//val cleaned = grouped.mapValues(v1 => {
//  v1.flatMap(v2 => {
//    v2._2
//  }).distinct
//})
//
//val findal = TreeMap(cleaned.toArray:_*)
import sorting.Point2D._
import geometries.Point2D
import geometries.Point2DInterface
import geometries.Point2DImplicits._
val low = new Low[Point2D, Double]()
val left = new Left[Point2D, Double]()
val up = new High[Point2D, Double]()
val right = new Right[Point2D, Double]()
val a = Point2D(1.0, 1.0)
val b = Point2D(2.0, 2.0)
up.compare(a,b)