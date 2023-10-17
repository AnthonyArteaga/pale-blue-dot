package tests

import org.scalatest._
import pbd.PaleBlueDot
class ApplicationObjective extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"


  test("test 1"){

    assert(PaleBlueDot.closestCity(citiesFilename,List(42.55,1.5333333)).sorted == List("ad","ordino","05").sorted, "ad, exact coordinate")

    assert(PaleBlueDot.closestCity(citiesFilename,List(-65.705004, 73.543403)).sorted == List("tf", "port-aux-francais", "00").sorted)

    assert(PaleBlueDot.closestCity(citiesFilename, List(42.741314, -78.507511)).sorted == List("us", "lancaster", "NY").sorted)

    assert(PaleBlueDot.closestCity(citiesFilename, List(-90, -180)).sorted == List("ar", "ushuaia", "23").sorted)

  }

}
