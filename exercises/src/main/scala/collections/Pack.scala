package collections

object Pack {
  // Hint : takeWhile/dropWhile or span
  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case h :: t =>
      val list1 = list.takeWhile(e => e == h)
      val rest = list.dropWhile(e => e == h)
      val listRest = pack(rest)
      list1 :: listRest
  }

  def encode[T](list: List[T]): List[(T, Int)] =
    pack(list).map(l => (l.head, l.size))
}
