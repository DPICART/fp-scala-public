class PathHelper {

  def shortestPath(paths: Vector[Path]): Option[Path] = paths match {
    case Vector() => None
    case paths => {
      if (paths.contains(Path(Vector())))
        None
      else
        Some(paths.minBy(p => p.length))
    }

  }

  def willVisit(paths: Vector[Path], point: Point): Vector[Path] = paths match {
    case Vector() => Vector()
    case paths => {
      if (paths.contains(Path(Vector())))
        Vector()
      else
        paths.filter(p => p.stops.contains(point))
    }
  }

  def willVisitAllPoints(paths: Vector[Path], points: Vector[Point]): Vector[Path] = paths match {
    case Vector() => Vector()
    case paths => {
      if (paths.contains(Path(Vector())))
        Vector()
      else
        //paths.filter(p => p.stops.forall(s => points.contains(s)))
        paths.filter(path => points.forall(point => path.stops.contains(point)))
    }
  }

  def bestPath(paths: Vector[Path], points: Vector[Point]): Option[Path] = paths match {
    case Vector() => None
    case paths    => shortestPath(willVisitAllPoints(paths, points))
  }

  //Return the list of segments which can be used from a given point
  // USELESS
  def segmentsCanBeAppliedOnPoint(point: Point, segments: Vector[Segment]) = {
    segments.filter(s => s.from == point)
  }

  //USELESS
  def applySegmentToPoint(point: Point, segment: Segment): Point = {
    Point("", point.x + segment.to.x, point.y + segment.to.y)
  }

  def findAllPossiblePaths(begin: Point, end: Point, segments: Vector[Segment]): Vector[Path] = segments match {
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

  def bestPathWrapUp(segments: Vector[Segment], from: Point, to: Point, stops: Vector[Point] = Vector()): Option[Path] = {
    bestPath(findAllPossiblePaths(from, to, segments), stops)
  }
}
