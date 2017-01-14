import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object ClusteringHamming {

  class Edge(inputNode1: Int, inputNode2: Int, inputCost: Int) extends Ordered[Edge] {
    var vertex1: Int = inputNode1
    var vertex2: Int = inputNode2
    var cost: Int = inputCost

    def compare(that: Edge) = this.cost - that.cost

    override def toString: String =
      "(" + vertex1 + " -> " + vertex2 + ", cost: " + cost + ")"
  }


  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
                val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/cluster_big_test.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/cluster_big_test2.txt"
//    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/cluster_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val str = iter.next().split(" ")
    val numNodes: Int = str(0).toInt
    val numBits: Int = str(1).toInt

    val nodeCluters: Array[Int] = 0 to (numNodes - 1) toArray
    var numClusters: Int = numNodes

    var edges: Array[Edge] = Array()
    var nodes: Array[Int] = Array()

    for (line <- iter) {
      val binaryStr = line.split(" ").mkString("")
      nodes = nodes :+ Integer.parseInt(binaryStr, 2)
    }

    for (i <- 0 to (numNodes - 1)) {
      for (j <- (i + 1) to (numNodes - 1)) {

        if (nodeCluters(i) != nodeCluters(j)) {
          val cost = Integer.bitCount(nodes(i) ^ nodes(j))
//          println(i + " - " + j + " : " + cost)
          if (cost < 3) {
            for (k <- 0 to (numNodes - 1)) {
              if (nodeCluters(k) == nodeCluters(j)) {
                nodeCluters(k) = nodeCluters(i)
              }
            }
          }
          //          println(nodeCluters.mkString(" ") + "\n")
        }

      }
    }


    println(nodeCluters.groupBy(l => l).keys.toArray.length)


  }
}
