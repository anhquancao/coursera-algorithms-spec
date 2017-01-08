import scala.io.Source

/**
  * Created by caoquan on 11/21/16.
  */
object TwoSum {


  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/Desktop/data.txt"
    // put data into a hash table
    var maps: Map[Long, Long] = Map()
    var count = 0
    for (line <- Source.fromFile(filename).getLines()) {
      val elem = line.toLong
      maps += (elem -> elem)
    }
    for (t <- -10000 to 10000) {
      println("value: " + t)
      maps.values.foreach {
        v => {
          if (maps.contains(t - v)) {
            count += 1
          }
        }
      }
    }
    println("count: " + count)
  }
}
