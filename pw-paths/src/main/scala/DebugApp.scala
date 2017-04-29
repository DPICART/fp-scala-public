object DebugApp extends App {

  val tl = Point("tl", 0, -10)
  val t = Point("t", 0, 0)
  val tr = Point("tr", 0, 10)
  val bl = Point("bl", 10, -10)
  val b = Point("b", 10, 0)
  val br = Point("br", 10, 20)

  // tl - t - tr
  //  |   |    \
  // bl - b --- br

  val segments = Vector(
    Segment(tl, tr),
    Segment(tr, br),
    Segment(br, tr),
    Segment(tr, tl),
    Segment(tl, bl),
    Segment(bl, br),
    Segment(br, bl),
    Segment(bl, tl))

  def end = br

  def findAllPossiblePathsTest(begin: Point, end: Point, segments: Vector[Segment]): Vector[Path] = segments match {
    case Vector() => Vector()
    case _ =>
      def findPathRec(newBegin: Point, currentPath: Path, segments: Vector[Segment]): Vector[Path] = segments match {
        case Vector() => Vector(currentPath)
        case _ =>
          if (segments.filter(s => s.from == newBegin) == Vector()) {
            Vector(currentPath)
          } else if (newBegin == end) {
            Vector(currentPath)
          } else {
            segments.filter(s => s.from == newBegin).flatMap(s => findPathRec(s.to, new Path(currentPath.segments :+ (s)), segments.filter(ss => ss != s)))
          }
      }
      segments.flatMap(p => findPathRec(begin, new Path(Vector()), segments).filter(p => p.segments.last.to == end)).distinct
  }

  println(findAllPossiblePathsTest(tl, br, segments))

}
