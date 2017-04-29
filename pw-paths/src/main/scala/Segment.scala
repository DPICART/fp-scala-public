case class Point(name: String, x: Long, y: Long)

case class Segment(from: Point, to: Point) {
  def distance: Double = {
    scala.math.sqrt(scala.math.pow((from.x.doubleValue() - to.x.doubleValue()), 2) + scala.math.pow((from.y.doubleValue() - to.y.doubleValue()), 2))
  }
}

case class Path(segments: Vector[Segment]) {

  def length: Double = {
    segments.map(s => s.distance).sum
  }

  //give all the possible stops on a given path
  def stops: Vector[Point] = {
    segments.map(s => s.from).union(segments.map(s => s.to)).distinct
  }
}
