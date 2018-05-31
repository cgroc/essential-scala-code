package stdlib.film

final case class Director(
  firstName: String,
  lastName: String,
  yearOfBirth: Int,
  films: List[Film]
) {
  def name: String =
    s"$firstName $lastName"
}

final case class Film(
  name: String,
  yearOfRelease: Int,
  imdbRating: Double
)

object TestData {
  private val memento           = new Film("Memento", 2000, 8.5)
  private val darkKnight        = new Film("Dark Knight", 2008, 9.0)
  private val inception         = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven        = new Film("Unforgiven", 1992, 8.3)
  val granTorino        = new Film("Gran Torino", 2008, 8.2)
  val invictus          = new Film("Invictus", 2009, 7.4)

  val predator          = new Film("Predator", 1987, 7.9)
  val dieHard           = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    List(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    List(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    List(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Nil)

  val directors = List(someGuy, mcTiernan, nolan, eastwood)
}

object TestMethods {
  import TestData.directors

  def directorsWithBackCatalogOfSize(numberOfFilms: Int): List[Director] =
    directors.filter(d => d.films.size >= numberOfFilms)

  def directorsBornBefore(year: Int): List[Director] =
    directors.filter(d => d.yearOfBirth < year)

  def directorsBornBeforeWithBackCatalogOfSize(year: Int, numberOfFilms: Int): List[Director] =
    directorsBornBefore(year).intersect(directorsWithBackCatalogOfSize(numberOfFilms)) // Oooh! inefficient! Better to reuse the comparisonfunctions!

  def namesOfAllFilmsByAllDirectors: List[String] =
    for {
      d <- directors
      f <- d.films
    } yield f.name

  def namesOfFilmsByNolan: List[String] =
    for {
      d <- directors.filter(dr => (dr.firstName == "Christopher" && dr.lastName == "Nolan"))
      f <- d.films
    } yield f.name

  def averageImdbRating: Double = {
    val allFilms = for { // look at the spec file! You don't actually need to do a for comp, a simple flatMap will work
      d <- directors
      f <- d.films
    } yield f
    allFilms.map(_.imdbRating).sum / allFilms.length
  }

  def directorsSortedByAge(ascending: Boolean = true): List[Director] =
    directors.sortWith((d1, d2) => if(ascending) d1.yearOfBirth < d2.yearOfBirth else d1.yearOfBirth > d2.yearOfBirth)

  def allFilmsSortedByImdb: List[Film] =
    directors.flatMap(_.films).sortWith(_.imdbRating > _.imdbRating)

  def earliestFilmByAnyDirector: Option[Film] =
    directors.flatMap(_.films).sortWith(_.yearOfRelease < _.yearOfRelease).headOption

  def earliestFilmsByAllDirectors: Map[Director, Option[Film]] =
    directors.map(d => (d, d.films.sortBy(_.yearOfRelease).headOption)).toMap
}

object Main extends App {
  import TestData.directors
  import TestMethods._

  // println("""directorsWithBackCatalogOfSize(3)                 == """ + directorsWithBackCatalogOfSize(3))
  // println("""directorsBornBefore(1970)                         == """ + directorsBornBefore(1970))
  // println("""directorsBornBeforeWithBackCatalogOfSize(1970, 5) == """ + directorsBornBeforeWithBackCatalogOfSize(1970, 5))
  // println("""namesOfFilmsByNolan                               == """ + namesOfFilmsByNolan)
  // println("""namesOfAllFilmsByAllDirectors                     == """ + namesOfAllFilmsByAllDirectors)
  // println("""averageImdbRating                                 == """ + averageImdbRating)
  // println("""directorsSortedByAge(false)                       == """ + directorsSortedByAge(false))
  // println("""allFilmsSortedByImdb                              == """ + allFilmsSortedByImdb)
  // println("""earliestFilmsByAllDirectors                       == """ + earliestFilmsByAllDirectors)
  // println("""earliestFilmByAnyDirector                         == """ + earliestFilmByAnyDirector)
}
