import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object FloydWarshall {

  class Edge(val head: Int, val tail: Int, val weight: Int) {
    override def toString: String =
      head + " -> " + tail + " : " + weight
  }

  def getCost(head: Int, tail: Int, edges: Array[Edge]): Int = {
    edges.foreach(e => {
      if (head == e.head && tail == e.tail) {
        return e.weight
      } else if (head == tail) {
        return 0
      }
    })
    Int.MaxValue
  }

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/g2.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val lineArray: Array[String] = iter.next().split(" ")
    val n: Int = lineArray(0).toInt
    val m: Int = lineArray(1).toInt

    var edges: Array[Edge] = Array()

    for (line <- iter) {
      val strArr = line.split(" ")
      val edge = new Edge(strArr(0).toInt, strArr(1).toInt, strArr(2).toInt)
      edges = edges :+ edge
    }

    val A: Array[Array[Array[Int]]] = Array.fill(n + 1) {
      Array.fill(n + 1) {
        Array.fill[Int](n + 1)(0)
      }
    }

    println("Init values")

    for (i <- 1 to n) {
      for (j <- 1 to n) {
        A(i)(j)(0) = getCost(i, j, edges)
      }
    }

    println("Fill in the table")

    for (k <- 1 to n) {
      println("k: " + k)
      for (i <- 1 to n) {
        println("i: " + i)
        for (j <- 1 to n) {
          A(i)(j)(k) = Math.min(A(i)(k)(k - 1), A(k)(j)(k - 1))
        }
      }
    }

    println("Check negative cycle")

    for (i <- 1 to n) {
      if (A(i)(i)(n) < 0) {
        println("Has Negative Cycle")
        return 1
      }
    }

    println("Compute min value")

    var minValue = A(0)(1)(n)

    for (i <- 1 to n) {
      for (j <- 1 to n) {
        if (minValue > A(i)(j)(n)) {
          minValue = A(i)(j)(n)
        }
      }
    }

    println(minValue)
  }

}
