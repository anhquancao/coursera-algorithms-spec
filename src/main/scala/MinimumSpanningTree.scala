import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object MinimumSpanningTree {

  class Edge(inputNode1: Int, inputNode2: Int, inputCost: Int) {
    var vertex1: Int = inputNode1
    var vertex2: Int = inputNode2
    var cost: Int = inputCost

    override def toString: String =
      "(" + vertex1 + " -> " + vertex2 + ", cost: " + cost + ")"
  }

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/edges.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/edgetest.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val firstLine = iter.next().split(" ")

    val numVertices: Int = firstLine(0).toInt
    val numEdges: Int = firstLine(1).toInt

    var edges: Array[Edge] = Array()

    for (line <- iter) {
      val strArr = line.split(" ")
      val edge1 = new Edge(strArr(0).toInt, strArr(1).toInt, strArr(2).toInt)
      val edge2 = new Edge(strArr(1).toInt, strArr(0).toInt, strArr(2).toInt)
      edges = edges :+ edge1
      edges = edges :+ edge2
    }

    var X: Array[Int] = Array(edges(0).vertex1)

    // Vertices spanned by tree-so-far T
    var T: Array[Edge] = Array()

    while (X.length != numVertices) {
      var minEdge: Edge = null
      for (edge <- edges) {
        if (X.contains(edge.vertex1) && !X.contains(edge.vertex2)) {
          if (minEdge == null) {
            minEdge = edge
          } else {
            if (edge.cost < minEdge.cost) {
              minEdge = edge
            }
          }
        }
      }
      T = T :+ minEdge
      X = X :+ minEdge.vertex2
      println(minEdge.vertex2 + " - " + minEdge.cost)
    }

    var totalCost: Int = 0

    for (edge <- T) {
      //      println(edge)
      totalCost += edge.cost
    }

    println("total cost: " + totalCost) // -3612829
  }
}
