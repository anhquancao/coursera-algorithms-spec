import scala.io.Source

/**
  * Created by caoquan on 11/21/16.
  */
object DijkstraAlgorithm {

  /**
    * Implementation of DijkstraAlgorithm
    *
    * @param edges    list of edges
    * @param vertices total number of vertices
    * @return Array[Int] array of shortest path
    */
  def findShortestPath(edges: Array[(Int, Int, Int)], vertices: Array[Int]): Array[Int] = {

    // vertices processed
    var X: Array[Int] = Array(vertices(0))

    // compute shorted path distance
    val A: Array[Int] = Array.fill(vertices.length + 1)(0)

    for (vertex <- vertices) {
      var minLength = 1000000
      var minVertex = 0

      for (edge <- edges) {

        // if the left vertex in X(processed) and the
        // right vertex not in X (not processed)
        if (X.contains(edge._1) && !X.contains(edge._2)) {
          val temp = A(edge._1) + edge._3
          if (temp < minLength || minVertex == 0) {
            minLength = temp
            minVertex = edge._2
          }
        }
      }
      X = X :+ minVertex
      A(minVertex) = minLength
    }
    A
  }

  def main(args: Array[String]): Unit = {

    // Read data from the file
    val filename = "/Users/caoquan/Desktop/text.txt"
    // array of edges with length
    var edges: Array[(Int, Int, Int)] = Array()
    // array of vertices
    var vertices: Array[Int] = Array()

    // extract data for each line from text file
    for (line <- Source.fromFile(filename).getLines()) {
      val arr = line.split('\t')
      // left vertex
      val left = arr(0).toInt
      // add vertice to array
      vertices = vertices :+ left

      // process the data on line
      for (i <- 1 until arr.length) {
        val rightAndLength = arr(i).split(',') // right vertex and length of the edge
        edges = edges :+ (left, rightAndLength(0).toInt, rightAndLength(1).toInt)
      }

    }

    val shortestPathValues = findShortestPath(edges, vertices)
    println(shortestPathValues(7) + "," + shortestPathValues(37) +
      "," + shortestPathValues(59) + "," + shortestPathValues(82) +
      "," + shortestPathValues(99) + "," + shortestPathValues(115) +
      "," + shortestPathValues(133) + "," + shortestPathValues(165) +
      "," + shortestPathValues(188) + "," + shortestPathValues(197))
    //    var i = 0
    //    for (value <- shortestPathValues) {
    //      println(i + " " + value)
    //      i += 1
    //    }
  }
}
