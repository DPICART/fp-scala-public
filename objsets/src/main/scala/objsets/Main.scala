package objsets

/**
 * Created by mdaviot on 6/13/16.
 */
object Main extends App {
  // Some help printing the results:
  println("RANKED:")
  GoogleVsApple.trending foreach println
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  /*
   * TweetReader.allTweets
   * .filter
   *google.exist( text )
   */

  val googleTweets: TweetSet = TweetReader.allTweets.filter(tweetTest => google.exists(e => tweetTest.text.contains(e)))

  val appleTweets: TweetSet = TweetReader.allTweets.filter(tweetTest => apple.exists(e => tweetTest.text.contains(e)))

  /**
   * On regroupe les deux listes avec le .union
   * Puis on les classe grâce a .ascendingByRetweet
   *
   */
  val trending: Trending = (appleTweets.union(googleTweets)).ascendingByRetweet
}
