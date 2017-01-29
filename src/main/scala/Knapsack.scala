import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object Knapsack {

  class Item(val value: Int, val weight: Int) {

    override def toString: String =
      "(value: " + value + ", weight: " + weight + ")"
  }


  def main(args: Array[String]): Unit = {

    val k: Int = 4
    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_test.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val lineArray: Array[String] = iter.next().split(" ")
    val knapSize: Int = lineArray(0).toInt
    val numItems: Int = lineArray(1).toInt

    var items: Array[Item] = Array()

    for (line <- iter) {
      val strArr = line.split(" ")
      val item = new Item(strArr(0).toInt, strArr(1).toInt)
      items = items :+ item
    }

    val A = Array.ofDim[Int](numItems + 1, knapSize + 1)

    for (x <- 0 to knapSize) {
      A(0)(x) = 0
    }

    for (i <- 1 to numItems) {
      for (x <- 0 to knapSize) {
        val case1: Int = A(i - 1)(x)
        if (x - items(i - 1).weight >= 0) {
          val case2 = A(i - 1)(x - items(i - 1).weight) + items(i - 1).value
          A(i)(x) = Math.max(case1, case2)
        } else {
          A(i)(x) = case1
        }
      }
    }

    println(A(numItems)(knapSize))
    // 2493893
  }
}
