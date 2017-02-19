import scala.io.Source
import scala.util.Random

/**
  * Created by caoquan on 11/21/16.
  */
object TravelingSaleMan {

  class Edge(val head: Int, val tail: Int, val weight: Double) {
    override def toString: String =
      head + " -> " + tail + " : " + weight
  }

  class City(val x: Double, val y: Double, val number: Int) {
    override def toString: String =
      " ( " + number + " : " + x + " , " + y + " ) "
  }


  def distanceBetween(city: City, anotherCity: City) =
    Math.sqrt(Math.pow(city.x - anotherCity.x, 2) + Math.pow(city.y - anotherCity.y, 2))

  def computeTour(list: List[City]): Double = {
    val length = list.length
    var distance: Double = distanceBetween(list(length - 1), list(0))
    for (i <- 1 until length) {
      distance += distanceBetween(list(i - 1), list(i))
    }
    distance
  }

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/tsp.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val numberOfCities: Int = iter.next().toInt

    var cities: Array[City] = Array()
    var index = 0
    for (line <- iter) {
      val strArr = line.split(" ")
      val city = new City(strArr(0).toDouble, strArr(1).toDouble, index)
      index += 1
      cities = cities :+ city
    }

    //    var edges: Array[Edge] = Array()
    //    cities.foreach((city: City) => {
    //      cities.foreach((anotherCity: City) => {
    //        if (city.number != anotherCity.number) {
    //          val edge = new Edge(city.number, anotherCity.number, euclideanDistance(city, anotherCity))
    //          edges = edges :+ edge
    //        }
    //
    //      })
    //    })
    var citiesList: List[City] = cities.toList
    var minDistance = computeTour(citiesList)

    for (i <- 1 to 1000000000) {
      citiesList = Random.shuffle(cities.toList).take(numberOfCities)
      val newDistance = computeTour(citiesList)
      if (newDistance < minDistance) {
        minDistance = newDistance
        println(minDistance)
      }
    }

    println("Min distance: " + minDistance)
    // 26442


  }

}
