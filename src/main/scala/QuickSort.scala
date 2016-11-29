import scala.io.Source

/**
  * Created by caoquan on 11/29/16.
  */
object QuickSort {
  var total = 0

  def choosePivot(a: Array[Int]): Int = {
    val len = a.length
    var middle = len / 2
    if (len % 2 == 0) middle = len / 2 - 1
    val firstVal = a(0)
    val lastVal = a(len - 1)
    val middleVal = a(middle)
    val median = Math.max(Math.min(firstVal, lastVal), Math.min(Math.max(firstVal, lastVal), middleVal))
    if (median == firstVal) 0
    else if (median == lastVal) len - 1
    else middle
  }

  def quickSort(a: Array[Int]): Array[Int] = {
    if (a.length <= 1) a
    else {
      val pivot: Int = choosePivot(a)
      val pivotVal = a(pivot)
      //      println("quickSort: " + getString(a))
      //      println("pivot: " + pivotVal)
      val partitionResult = partition(a, pivot)
      val left = quickSort(partitionResult._1)
      val right = quickSort(partitionResult._2)
      val result = left ++ Array(pivotVal) ++ right
      result
    }
  }

  def partition(a: Array[Int], pi: Int): (Array[Int], Array[Int]) = {
    total += a.length - 1
    val temp1 = a(0)
    a(0) = a(pi)
    a(pi) = temp1
    val pivot = 0

    val p = a(pivot)
    var i = pivot + 1
    for (j <- pivot + 1 until a.length) {
      if (a(j) < p) {
        val temp = a(i)
        a(i) = a(j)
        a(j) = temp
        i += 1
      }
    }
    val temp = a(pivot)
    a(pivot) = a(i - 1)
    a(i - 1) = temp

    (a.slice(0, i - 1), a.slice(i, a.length))
  }

  def getString(array: Array[Int]): String = {
    return "[" + array.mkString(", ") + "]"
  }

  def main(args: Array[String]): Unit = {
    //    val res = partition(Array(3, 2, 1, 8, 4, 6), 0)
    //    println(getString(res._1))
    //    println(getString(res._2))
    val test10 = Array(3, 9, 8, 4, 6, 10, 2, 5, 7, 1)
    val test100 = Array(57, 97, 17, 31, 54, 98, 87, 27, 89, 81, 18, 70, 3, 34, 63, 100, 46, 30, 99, 10, 33, 65, 96, 38, 48, 80, 95,
      6, 16, 19, 56, 61, 1, 47, 12, 73, 49, 41, 37, 40, 59, 67, 93, 26, 75, 44, 58, 66, 8, 55, 94, 74, 83, 7,
      15, 86, 42, 50, 5, 22, 90, 13, 69, 53, 43, 24, 92, 51, 23, 39, 78, 85, 4, 25, 52, 36, 60, 68, 9, 64, 79,
      14, 45, 2, 77, 84, 11, 71, 35, 72, 28, 76, 82, 88, 32, 21, 20, 91, 62, 29)
    //    val res1 = quickSort(test10)
    //    println(getString(res1))

    //    val res2 = quickSort(test100)
    //    println(getString(res2))
    val filename = "/Users/caoquan/Desktop/text.txt"
    var inputArray: Array[Int] = Array[Int]()
    for (line <- Source.fromFile(filename).getLines()) {
      inputArray = inputArray :+ line.toInt
    }
    println(inputArray.length)
    println(getString(quickSort(inputArray)))
    println(total)
  }
}
