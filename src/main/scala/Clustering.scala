import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object Clustering {

  class Edge(inputNode1: Int, inputNode2: Int, inputCost: Int) extends Ordered[Edge] {
    var vertex1: Int = inputNode1
    var vertex2: Int = inputNode2
    var cost: Int = inputCost

    def compare(that: Edge) = this.cost - that.cost

    override def toString: String =
      "(" + vertex1 + " -> " + vertex2 + ", cost: " + cost + ")"
  }


  def main(args: Array[String]): Unit = {

    val k: Int = 4
    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/clustering1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/edgetest.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val numNodes: Int = iter.next().toInt
    val nodeCluters: Array[Int] = 0 to (numNodes - 1) toArray
    var numClusters: Int = numNodes

    var edges: Array[Edge] = Array()

    for (line <- iter) {
      val strArr = line.split(" ")
      val edge = new Edge(strArr(0).toInt, strArr(1).toInt, strArr(2).toInt)
      edges = edges :+ edge
    }

    while (numClusters != k) {
      var tempEdges: Array[Edge] = edges.filter(e => nodeCluters(e.vertex1 - 1) != nodeCluters(e.vertex2 - 1))
      Sorting.quickSort(tempEdges)
      val minEdge = tempEdges(0)

      val fromCluster = nodeCluters(minEdge.vertex2 - 1)
      val toCluster = nodeCluters(minEdge.vertex1 - 1)

      for (i <- 0 to (numNodes - 1)) {
        //        println(nodeCluters(i))

        if (nodeCluters(i) == fromCluster) {
          nodeCluters(i) = toCluster
        }
      }
      //      println("merge: " + minEdge)
      //      println(nodeCluters.mkString(" "))
      //      println("----")
      numClusters -= 1
    }
    val tempEdges: Array[Edge] = edges.filter(e => nodeCluters(e.vertex1 - 1) != nodeCluters(e.vertex2 - 1))
    Sorting.quickSort(tempEdges)
    //    for (edge <- tempEdges) {
    //      println(edge)
    //    }
    println(tempEdges(0)) // 106

  }
}
