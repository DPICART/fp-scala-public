package recursivity

import scala.annotation.tailrec

object CountChange {

  def countChange(money: Int, coins: List[Int]): Int =
  {
    if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else if(money == 0)
      1
    else
      0
  }
}