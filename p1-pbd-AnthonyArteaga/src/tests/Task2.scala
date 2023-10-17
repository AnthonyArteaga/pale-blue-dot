package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  val epsilon: Double = .001
  def isEqualDouble(d1: Double, d2: Double): Boolean = {
    if(Math.abs(d1-d2) < epsilon)
      true
    else
      false
  }

  test("1st test") {

    assert(isEqualDouble(PaleBlueDot.averagePopulation(countriesFile,citiesFilename,"Andorra"), 8409.5),"ad")

    assert(isEqualDouble(PaleBlueDot.averagePopulation(countriesFile,citiesFilename,"united ARAB eMiRaTeS"), 761668.333), "ae")

    assert(isEqualDouble(PaleBlueDot.averagePopulation(countriesFile,citiesFilename,"MOZAMBIQUE"), 192239.70833), "mz")


  }


}
