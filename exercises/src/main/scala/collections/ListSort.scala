package collections

object ListSort {
  def isort(xs: List[Int]): List[Int] =
    xs match {
      case Nil    => Nil
      case h :: t => insert(h, isort(t))
    }

  def insert(x: Int, xs: List[Int]): List[Int] =
    xs match {
      case Nil => x :: Nil
      case h :: t => if (h < x) {
        h :: insert(x, t)
      } else {
        x :: t
      }

    }
}
