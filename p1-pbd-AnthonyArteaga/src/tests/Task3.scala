package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test for cityPopulations") {

    assert(PaleBlueDot.cityPopulations(countriesFile, citiesFilename,"anDoRRa","04") == Map("la massana" -> 7211), "ad")

    assert(PaleBlueDot.cityPopulations(countriesFile, citiesFilename, "Afghanistan", "06") == Map("anar darreh" -> 10023, "farah" -> 43574 ), "af")

    val usMap: Map[String,Int] = Map(
      "barrington" -> 16669,
      "bristol" -> 22795,
      "central falls" -> 19509,
      "charlestown" ->8421,
      "coventry" ->35525,
      "cranston" ->82926,
      "cumberland" ->34843,
      "east greenwich" ->13682,
      "east providence" ->50453,
      "exeter" ->6426,
      "foster" ->4598,
      "greenville" ->8964,
      "hopkinton" ->8261,
      "jamestown" ->5755,
      "kingston" ->5113,
      "middletown" ->17303,
      "narragansett" ->17236,
      "newport" ->25818,
      "north providence" ->33835,
      "north smithfield" ->11212,
      "pascoag" ->4716,
      "pawtucket" ->74965,
      "portsmouth" ->17756,
      "providence" ->177595,
      "smithfield" ->21872,
      "tiverton" ->19432,
      "valley falls" ->12048,
      "warren" ->11280,
      "warwick" ->88071,
      "west warwick" ->30146,
      "westerly" ->26048,
      "woonsocket" ->45839
    )
    assert(PaleBlueDot.cityPopulations(countriesFile, citiesFilename,"United states of America", "RI") == usMap, "US, RI")

  }

  test("test for aboveAverageCities"){

    val citiesAD: List[String] = List("les escaldes")
    assert(PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"Andorra").sorted == citiesAD.sorted, "above average cities Andorra")

    val citiesMZ: List[String] = List("beira","chimoio","maputo","matola","nacala","nampula")
    assert(PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"MOZaMBIQUE").sorted == citiesMZ.sorted, "above average cities Mozambique")

    val citiesAE: List[String] = List("dubai")
    assert(PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, "united arab emirates").sorted == citiesAE.sorted, "above average cities AE")

    assert(PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, "singapore").sorted == List(), "singapore")


  }

}
