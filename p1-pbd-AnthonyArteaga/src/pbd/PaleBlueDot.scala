package pbd

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}

object PaleBlueDot {


  /**
   * Task 1
   *
   * Given a country name using a mix of case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename and the countryName input
   * of your method is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName in lowercase letters
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    val countriesFile: BufferedSource = Source.fromFile(countriesFilename)
    var code: String = ""

    for(line <- countriesFile.getLines()){
      val splits: Array[String] = line.split("#")
      if(splits(0).toLowerCase() == countryName.toLowerCase){
        code = splits(1)
      }
    }

    code.toLowerCase()
  }


  /**
   * Task 2
   *
   * Find the average population of cities in a country
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The average population of cities in the given country
   */
  def averagePopulation(countriesFilename: String, citiesFilename: String, countryName: String): Double = {

    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)

    val countryCode = getCountryCode(countriesFilename, countryName)

    var sum:Double = 0.0
    var cityCount: Int = 0

    for(line <- citiesFile.getLines()){
      val splits: Array[String]=line.split(",")
      if(splits(0) == countryCode){
        sum += splits(3).toDouble
        cityCount += 1
      }

    }

    sum/cityCount
  }


  /**
   * Task 3
   */

  /**
   * Returns a Map[cityName -> population] for all cities in the given county. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int. The country name may contain any mix of upper/lower-case letters.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @param regionCode        Two digit region code with case matching the case from the cities file
   * @return A Map containing the name and population of every city in the given country
   */
  def cityPopulations(countriesFilename: String, citiesFilename: String, countryName: String, regionCode: String): Map[String, Int] = {
    var outputMap: Map[String,Int]=Map()

    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)

    val countryCode = getCountryCode(countriesFilename, countryName)

    for(line <- citiesFile.getLines()){
      val splits: Array[String]=line.split(",")

      if (splits(0)==countryCode && splits(2)==regionCode){
        outputMap += (splits(1) -> splits(3).toInt)
      }

    }

    outputMap
  }


  /**
   * Returns a List of city names in the given county and with above average population for that country
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return All city names in given country with a population > the average populations of cities in that country
   */
  def aboveAverageCities(countriesFilename: String, citiesFilename: String, countryName: String): List[String] = {
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    val countryCode = getCountryCode(countriesFilename ,countryName)
    val countryAvg = averagePopulation(countriesFilename, citiesFilename, countryName)
    var outputList: List[String] = List()

    for(line <- citiesFile.getLines()){
      val splits = line.split(",")
      if(splits(0)==countryCode && splits(3).toInt > countryAvg){
        outputList = outputList :+ splits(1)
      }
    }

    outputList
  }


  /**
   * Application Objective
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * Return the closest city to the given location in terms of greater circle distance which is the shortest distance
   * needed to walk along the surface of the Earth to reach a city.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file (ie. the List should have exactly 3 values to return
   *         a single city
   */
  def closestCity(citiesFilename: String, location: List[Double]): List[String] = {
    var output: List[String]=List()
    val lat1: Double = location(0)
    val lon1: Double = location(1)
    var minDistance:Double= Double.MaxValue


    val citiesFile: BufferedSource= Source.fromFile(citiesFilename)

    for(line <- citiesFile.getLines()) {
      val splits: Array[String] = line.split(",")
      if (splits(4) != "Latitude") {

        val currLat: Double = splits(4).toDouble
        val currLon: Double = splits(5).toDouble
          if (greaterCircleDistance(lat1, lon1, currLat, currLon) < minDistance) { //maybe <=
            minDistance = greaterCircleDistance(lat1, lon1, currLat, currLon)

            output = List(splits(0), splits(1), splits(2))

          }

        }
      }


    //List("Country Code", "City Name", "Region")
    output
  }

  def greaterCircleDistance(lat1:Double, lon1: Double, lat2: Double, lon2:Double): Double={
    var R:Double = 6371; // km
    var φ1 = lat1 * Math.PI / 180; // φ, λ in radians
    var φ2 = lat2 * Math.PI / 180;
    var Δφ = (lat2 - lat1) * Math.PI / 180;
    var Δλ = (lon2 - lon1) * Math.PI / 180;

    var a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
      Math.cos(φ1) * Math.cos(φ2) *
        Math.sin(Δλ / 2) * Math.sin(Δλ / 2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

    var d = R * c; // in km
    d
  }


  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }


  def main(args: Array[String]): Unit = {
    openMap(List(43.002743, -78.7874136))
  }

}
