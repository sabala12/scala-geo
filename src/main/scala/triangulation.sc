import features.triangulation
import geometries.{Point2D, Polygon2D}

val l = List(234,1,5,345,23,2,5,6,34)
val polygon = Polygon2D(Point2D(2,1),
  Point2D(5,1),
  Point2D(9,2),
  Point2D(3,3),
  Point2D(6,3),
  Point2D(1,4),
  Point2D(2,6),
  Point2D(5,6),
  Point2D(7,9))

//val result = triangulation(polygon.points)