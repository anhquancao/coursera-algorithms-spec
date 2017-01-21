import scala.io.Source

/**
  * Created by caoquan on 11/21/16.
  */
object MaximumWeightIndependent {


  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    //        val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/mwis1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/mwis2.txt"
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/mwis.txt"

    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    var numVertices: Int = iter.next().toInt

    var weights: Array[Int] = Array()
    for (line <- iter) {
      weights = weights :+ line.toInt
    }
    var A: Array[Int] = new Array[Int](numVertices + 1)
    A(0) = 0
    A(1) = weights(0)
    for (i <- 2 to numVertices) {
      A(i) = Math.max(A(i - 1), A(i - 2) + weights(i - 1))
    }
    println("Max sum: " + A(numVertices))

    var S: Array[Int] = Array()
    var i = numVertices

    while (i > 1) {
      if (A(i - 1) >= A(i - 2) + weights(i - 1)) {
        i -= 1
      } else {
        S = S :+ i
        i -= 2
      }
    }
    val temp = Array(1, 2, 3, 4, 17, 117, 517, 997)
    var str = ""
    S.foreach(println)
    temp.foreach(t => {

      if (S.contains(t)) {
        str += 1
      } else {
        str += 0
      }
    })
    println(str)
    // result: 10100110

  }
}
