package collections

object Folds {
  def myLength[T](l: List[T]): Int =
    l.foldLeft(0) { case (count, t) => count + 1 }

  def myMap[T, U](list: List[T])(f: T => U): List[U] =
    list.foldRight(List.empty[U]) {
      case (elt, acc) =>
        f(elt) :: acc
    }

}
