import scala.io.Source
import scala.util.Random

/**
  * Created by caoquan on 11/21/16.
  */
object TravelingSaleManHeuristic {

  class City(val x: Double, val y: Double, val number: Int) {
    override def toString: String =
      " ( " + number + " : " + x + " , " + y + " ) "
  }


  def distanceBetween(city: City, anotherCity: City) =
    Math.sqrt(Math.pow(city.x - anotherCity.x, 2) + Math.pow(city.y - anotherCity.y, 2))

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/nn.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val numberOfCities: Int = iter.next().toInt

    var cities: List[City] = List()
    var index = 0
    for (line <- iter) {
      val strArr = line.split(" ")
      val city = new City(strArr(1).toDouble, strArr(2).toDouble, strArr(0).toInt)
      index += 1
      cities = cities :+ city
    }


    var currentCity = cities.head

    var startCity = currentCity

    var visitedCity: List[City] = List(currentCity)

    var totalDistance: Double = 0


    cities = cities.filter(city => city.number != 1)

    while (visitedCity.length < numberOfCities) {
      var minCity = cities.head
      var minDistance = distanceBetween(currentCity, minCity)
      for (elem <- cities) {
        val distance = distanceBetween(currentCity, elem)
        if (distance < minDistance) {
          minCity = elem
          minDistance = distance
        } else if (distance == minDistance) {
          if (minCity.number > elem.number) {
            minCity = elem
            minDistance = distance
          }
        }
      }
      println(currentCity.number + " -> " + minCity.number + ": " + minDistance)
      totalDistance += minDistance
      cities = cities.filter(city => city.number != minCity.number)
      visitedCity = minCity :: visitedCity
      currentCity = minCity
    }
    val minDistance = distanceBetween(currentCity, startCity)
    println(currentCity.number + " -> " + startCity.number + ": " + minDistance)
    totalDistance += minDistance
    println("distance: " + totalDistance)
    //1203344
  }

}
