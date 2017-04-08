package collections

import moviesdb.Movies.{ movies, titles }
import moviesdb.Actors.actors
import moviesdb._

object MoviesUtil {
  // Hint : contains
  def isInTop250(title: String): Boolean =
    titles.contains(title)

  // Hint : filter
  def findTitlesByYear(year: String): Vector[String] =
    titles.filter(t => t.contains("(" + year + ")"))
  // 2. rewrite later with a for expression

  // Hint : filter
  def findMoviesWithRatingAtLeast(rating: Float): Vector[Movie] =
    movies.filter(m => m.rating >= rating)

  // Hint : partition
  //(movies.filter(m => m.rating <= rating), movies.filter(m => m.rating > rating))
  def partitionMoviesAtRating(rating: Float): (Vector[Movie], Vector[Movie]) =
    movies.partition(m => m.rating <= rating)

  // Hint : maxBy
  def bestMovie: Movie =
    movies.maxBy(m => m.rating)

  // Hint : sortBy, reverse, take
  def topMovies(n: Int): Vector[Movie] =
    movies.sortBy(m => m.rating).reverse.take(n)

  def actorByName(actorName: String) = {
    // Hint : filter, head
    //actors.filter(a => a.name == actorName).headOption
    actors.find(a => a.name == actorName)
    // 2. what happens if no actor is found ? use headOption instead
    // 3. optimize with find
  }

  // Hint : same as previous
  def movieByTitle(title: String) = {
    movies.find(m => m.title == title)
  }

  def actorsInMovie(title: String): Vector[Actor] =
    actors.filter(a => a.movies.contains(title))

  def actorsInMoviePart(part: String): Vector[Actor] = {
    // Hint : filter, exists
    actors.filter(a => a.movies.exists(m => m.contains(part)))
    // 2. rewrite with a for expression (hint : also use distinct)
  }

  def moviesWith(actorName: String): Vector[Movie] =
    actorByName(actorName) match {
      case None                  => Vector()
      case Some(Actor(_, films)) => movies.filter(m => films.contains(m.title))
    }

  // Hint : ++, distinct
  def moviesWithAny(actorName1: String, actorName2: String): Vector[Movie] =
    (moviesWith(actorName1) ++ moviesWith(actorName2)).distinct

  // Hint : map
  def titlesWith(actorName: String): Vector[String] =
    moviesWith(actorName).map(m => m.title)

  //Hint : map, mkString
  def csv(movies: Vector[Movie]) = {
    movies.map(_.title).mkString(", ")
  }

  // Hint : ++, distinct (reuse titlesWith)
  def titlesWithAny(actorName1: String, actorName2: String): Vector[String] =
    (titlesWith(actorName1) ++ titlesWith(actorName2)).distinct

  // Hint : filter, forall /!\ (reuse titlesWith)
  def moviesWithAll(actorNames: Vector[String]): Vector[Movie] =
    movies.filter(m => actorNames.forall(a => titlesWith(a).contains(m)))

  def moviesWithAny(actorNames: Vector[String]): Vector[Movie] = {
    // Hint : filter, exists
    movies.filter(m => actorNames.exists(a => titlesWith(a).contains(m)))
    // 2. Rewrite using map, flatten and distinct instead

    // 3. Rewrite using flatMap and distinct instead
  }

  // Hint : map (reuse moviesWithAll)
  def titlesWithAll(actorNames: Vector[String]): Vector[String] =
    moviesWithAll(actorNames).map(_.title)

  // Hint : map, max (reuse moviesWith)
  def maxRating(actorName: String): Float = {
    ratings(actorName).max
  }

  def ratings(actorName: String): Vector[Float] =
    moviesWith(actorName).map(m => m.rating)

  // Hint : map, sum, size (reuse moviesWith)
  def avgRating(actorName: String): Float = {
    ratings(actorName).sum / ratings(actorName).size
  }

  // Hint : maxBy, isEmpty (reuse moviesWith)
  def bestMovie(actorName: String): Option[Movie] = {
    moviesWith(actorName) match {
      case Vector() => None
      case vector   => Some(vector.maxBy(m => m.rating))
    }
  }
}
