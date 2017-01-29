import scala.io.Source

/**
  * Created by caoquan on 11/21/16.
  */
object KnapsackBig {

  class Item(val value: Int, val weight: Int) {

    override def toString: String =
      "(value: " + value + ", weight: " + weight + ")"
  }


  def main(args: Array[String]): Unit = {

    val k: Int = 4
    // Read data from the file
    println("Input: ")
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_test.txt"
    //        val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
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

    val A: Array[Int] = new Array[Int](knapSize + 1)
    val A1: Array[Int] = new Array[Int](knapSize + 1)

    for (i <- 1 to numItems) {
      for (x <- 0 to knapSize) {
        if (i % 2 == 0) {
          val case1: Int = A(x)
          if (x - items(i - 1).weight >= 0) {
            val case2 = A(x - items(i - 1).weight) + items(i - 1).value
            A1(x) = Math.max(case1, case2)
          } else {
            A1(x) = case1
          }
        } else {
          val case1: Int = A1(x)
          if (x - items(i - 1).weight >= 0) {
            val case2 = A1(x - items(i - 1).weight) + items(i - 1).value
            A(x) = Math.max(case1, case2)
          } else {
            A(x) = case1
          }
        }

      }
    }


    println(A(knapSize))
    println(A1(knapSize)) // 4243395

  }
}
