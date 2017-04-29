import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PathSpec extends FlatSpec with Matchers {

  val tl = Point("tl", 0, -10)
  val t = Point("t", 0, 0)
  val tr = Point("tr", 0, 10)
  val bl = Point("bl", 10, -10)
  val b = Point("b", 10, 0)
  val br = Point("br", 10, 20)

  // tl - t - tr
  //  |   |     \
  // bl - b --- br

  val points = Vector(tl, t, tr, bl, b, br)

  val tlt = Segment(tl, t)
  val tb = Segment(t, b)
  val tlb = Segment(tl, b)
  val tt = Segment(t, t)
  val ttr = Segment(t, tr)
  val trbr = Segment(tr, br)
  val topPath = Path(Vector(tlt, ttr))
  val blb = Segment(bl, b)
  val bbr = Segment(b, br)
  val botPath = Path(Vector(blb, bbr))

  "a segment" should "compute its distance" in {
    val distance = tlt.distance
    distance shouldBe 10.0 +- 0.0001
  }

  "a trivial segment" should "should have a computed distance of 0" in {
    val distance = tt.distance
    distance shouldBe 0.0
  }

  "a vertical segment" should "should have a computed distance equals to the absolute value of the difference between y1 and y2" in {
    val distance = tlt.distance
    distance shouldBe scala.math.abs(tl.y - t.y)
  }

  "a general segment" should "should have a distance of 14.1421 +- 0.0001" in {
    val distance = tlb.distance
    distance shouldBe 14.1421 +- 0.0001
  }

  "an empty path" should "have a null length" in {
    val length = Path(Vector()).length
    length shouldBe 0
  }

  "a path containing one segment" should "compute its length" in {
    val length = Path(Vector(tlt)).length
    length shouldBe 10
  }

  "a path containing at least two segments" should "compute its length" in {
    val length = Path(Vector(tlt, ttr)).length
    length shouldBe 20
  }

  val helper = new PathHelper

  //The shortest path of an empty list of paths
  "shortestPath of an empty list of path" should "return None" in {
    val listPaths = Vector()
    helper.shortestPath(listPaths) shouldBe None
  }

  //The shortest path of a list of paths containing only empty paths
  "shortestPath of a list of empty paths" should "return None" in {
    val listPaths = Vector(Path(Vector()), Path(Vector()))
    helper.shortestPath(listPaths) shouldBe None
  }

  //The shortest path of a list of paths containing a single path
  val path_tltbbr = Path(Vector(tlt, tb, bbr))
  "shortestPath of a list of one path" should "be path_tltbbr" in {
    val listPaths = Vector(path_tltbbr)
    helper.shortestPath(listPaths) shouldBe Some(path_tltbbr)
  }

  val path_tlttrbr = Path(Vector(tlt, ttr, trbr))
  //The shortest path of a list of paths containing at least two paths
  "shortestPath of a list of two paths" should "be path_tltbbr" in {
    val listPaths = Vector(path_tltbbr, path_tlttrbr)
    helper.shortestPath(listPaths) shouldBe Some(path_tlttrbr)
  }

  //All stops of an empty path
  "All stops of an empty path" should "be an empty vector" in {
    val path = Path(Vector())
    path.stops shouldBe Vector()
  }

  //All stops of a path containing a single element
  "All stops of a path containing a single element" should "contains a vector of 2 points" in {
    val path = Path(Vector(ttr))
    path.stops shouldBe Vector(t, tr)
  }

  //All stops of a path containing at least two segments
  "All stops of a path containing at least two segments" should "contains a vector of at least 3 points" in {
    val path = Path(Vector(ttr, trbr))
    path.stops shouldBe Vector(t, tr, br)
  }

  //Filter an empty list of paths
  "Filter an empty list of paths" should "return an empty vector" in {
    val paths = Vector()
    val point = Point("Yo", 0, 0)
    helper.willVisit(paths, point) shouldBe Vector()
  }

  //Filter a list of paths containing paths which does not include the stop
  "Filter a list of paths containing paths which does not include the stop" should "return an empty vector" in {
    val paths = Vector(topPath, Path(Vector(tlt, tb, bbr)))
    val pointBotLeft = bl
    helper.willVisit(paths, pointBotLeft) shouldBe Vector()
  }

  //Filter a list of paths containing at least one path which include the stop
  "Filter a list of paths containing at least one path which include the stop" should "return a vector of at least one path" in {
    val paths = Vector(topPath, botPath)
    val pointBotLeft = bl
    helper.willVisit(paths, pointBotLeft) shouldBe Vector(botPath)
  }

  //Filter an empty list of paths
  "Filter an empty list of paths" should "return an empty Vector" in {
    val paths = Vector()
    val points = Vector(t, tl, b, br)
    helper.willVisitAllPoints(paths, points) shouldBe Vector()
  }

  // Filter a list of paths which does not stop at the full list of points
  "Filter a list of paths which does not stop at the full list of points" should "return an empty vector" in {
    val paths = Vector(botPath)
    val points = Vector(t, tl, b, br)
    helper.willVisitAllPoints(paths, points) shouldBe Vector()
  }

  //Filter a list of paths containing at list one path which stops at all the points
  "Filter a list of paths containing at list one path which stops at all the points" should "return a vector of path(s)" in {
    val topPath2 = Path(Vector(tlt, ttr, trbr))
    val paths = Vector(botPath, topPath, topPath2)
    val points = Vector(tl, t, tr)
    helper.willVisitAllPoints(paths, points) shouldBe Vector(topPath, topPath2)
  }

  //The best path of an empty list of paths
  "The best path of an empty list of paths" should "return None" in {
    val paths = Vector(Path(Vector()))
    val points = Vector(tl, t, tr)
    helper.bestPath(paths, points) shouldBe None
  }

  //The best path of a list of paths containing a single path which does not include the full list of points
  "The best path of a list of paths containing a single path which does not include the full list of points" should "return None" in {
    val topPath2 = Path(Vector(tlt, ttr, trbr))
    val paths = Vector(topPath, topPath2)
    val points = Vector(bl, b)
    helper.bestPath(paths, points) shouldBe None
  }

  //The best path of a list of paths containing a single path which includes the list of points
  "The best path of a list of paths containing a single path which includes the list of points" should "return a path" in {
    val topPath2 = Path(Vector(tlt, ttr, trbr))
    val paths = Vector(topPath, topPath2)
    val points = Vector(tl, t, tr, br)
    helper.bestPath(paths, points) shouldBe Some(topPath2)
  }

  //The best path of a list of paths containing at least two paths which include the list of points
  "The best path of a list of paths containing at least two paths which include the list of points" should "return the best path" in {
    val topPath3 = Path(Vector(tlt, tb, bbr))
    val paths = Vector(topPath, topPath3) //topPath est plus cours
    val points = Vector(tl, t)
    helper.bestPath(paths, points) shouldBe Some(topPath)
  }

  val pointDebut = Point("Point1", 0, 0)
  val point1 = Point("1", 0, 5)
  val point2 = Point("2", 20, 5)
  val point3 = Point("3", 20, 20)
  val pointFin = Point("Point2", 20, 20)

  val segment1 = Segment(pointDebut, point1)
  val segment2 = Segment(point1, point2)
  val segment3 = Segment(point2, point3)
  val segment4 = Segment(point3, pointFin)

  //Filter a list of segments, return the ones which can be apply from a given point
  "segmentsCanBeAppliedOnPoint - Segments not linked to a point" should "return an empty vector" in {
    helper.segmentsCanBeAppliedOnPoint(pointFin, Vector(segment1, segment2)) shouldBe Vector()
  }
  "segmentsCanBeAppliedOnPoint - Segments linked to a point" should "return a vector of segments which are linked to the point" in {
    helper.segmentsCanBeAppliedOnPoint(pointDebut, Vector(segment1, segment2)) shouldBe Vector(segment1)
  }
  //Create a new point given a point and a segment
  "applySegmentToPoint" should "return a new point" in {
    helper.applySegmentToPoint(pointDebut, Segment(pointDebut, pointFin)) shouldBe Point("", 20, 20)
  }

  //The possible paths when the segments are empty
  "findAllPossiblePaths - The possible paths when the segments are empty" should "return an empty vector" in {
    helper.findAllPossiblePaths(pointDebut, pointFin, Vector()) shouldBe Vector()
  }

  //The possible paths with a segment from the begin to the end
  "findAllPossiblePaths - The possible paths with a segment from the begin to the end" should "return an unique path" in {
    helper.findAllPossiblePaths(pointDebut, pointFin, Vector(Segment(pointDebut, pointFin))) shouldBe Vector(Path(Vector(Segment(pointDebut, pointFin))))
  }

  //The possible paths with segments without cycle
  "findAllPossiblePaths - The possible paths with a list of segment without cycle" should "return multiple paths" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(tl, bl),
      Segment(bl, br),
      Segment(tl, br))

    helper.findAllPossiblePaths(tl, br, segments) shouldBe Vector(Path(Vector(Segment(Point("tl", 0, -10), Point("tr", 0, 10)), Segment(Point("tr", 0, 10), Point("br", 10, 20)))), Path(Vector(Segment(Point("tl", 0, -10), Point("bl", 10, -10)), Segment(Point("bl", 10, -10), Point("br", 10, 20)))), Path(Vector(Segment(Point("tl", 0, -10), Point("br", 10, 20)))))
  }

  //The possible paths with a list of segment with cycle
  "findAllPossiblePaths - The possible paths with a list of segment with cycle" should "return multiple paths" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(br, tr),
      Segment(tr, tl),
      Segment(tl, bl),
      Segment(bl, br),
      Segment(br, bl),
      Segment(bl, tl))

    helper.findAllPossiblePaths(tl, br, segments) shouldBe Vector(Path(Vector(Segment(Point("tl", 0, -10), Point("tr", 0, 10)), Segment(Point("tr", 0, 10), Point("br", 10, 20)))), Path(Vector(Segment(Point("tl", 0, -10), Point("tr", 0, 10)), Segment(Point("tr", 0, 10), Point("tl", 0, -10)), Segment(Point("tl", 0, -10), Point("bl", 10, -10)), Segment(Point("bl", 10, -10), Point("br", 10, 20)))), Path(Vector(Segment(Point("tl", 0, -10), Point("bl", 10, -10)), Segment(Point("bl", 10, -10), Point("br", 10, 20)))), Path(Vector(Segment(Point("tl", 0, -10), Point("bl", 10, -10)), Segment(Point("bl", 10, -10), Point("tl", 0, -10)), Segment(Point("tl", 0, -10), Point("tr", 0, 10)), Segment(Point("tr", 0, 10), Point("br", 10, 20)))))
  }

  "bestPath" should "find the best path" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(tl, bl),
      Segment(bl, br))

    helper.bestPathWrapUp(segments, tl, br).get shouldBe Path(Vector(
      Segment(tl, tr),
      Segment(tr, br)))
  }

  "bestPath" should "find the best path with a stop" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(tl, bl),
      Segment(bl, br))

    helper.bestPathWrapUp(segments, tl, br, Vector(bl)).get shouldBe Path(Vector(
      Segment(tl, bl),
      Segment(bl, br)))
  }

}
