import geometries._
import features._

object main extends App {
  val l = List(234,1,5,345,23,2,5,6,34)
  val polygon = Polygon2D(Point2D(2.0,1.0),
                          Point2D(5.0,1.0),
                          Point2D(9.0,2.0),
                          Point2D(3.0,3.0),
                          Point2D(6.0,3.0),
                          Point2D(1.0,4.0),
                          Point2D(2.0,6.0),
                          Point2D(5.0,6.0),
                          Point2D(7.0,9.0))

//val polygon = Polygon2D(Point2D(1.0,1.0),
//  Point2D(4.0,6.0),
//  Point2D(5.0,6.0),
//  Point2D(0.0,1.0))

  val result = polygon.triangulate
  val a = 4
  //val x = A.top(5,l)
  //println(x)
}