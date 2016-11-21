import scala.io.Source
import scala.util.control.Breaks

/**
  * Created by caoquan on 11/21/16.
  */
object CountingInversion {

  def CountSplitInv(arrLeft: Array[Int], arrRight: Array[Int]): (Array[Int], Long) = {


    var i = 0
    var j = 0
    var d = Array[Int]()
    var count = 0


    while (i != arrLeft.length && j != arrRight.length) {
      if (arrLeft(i) <= arrRight(j)) {
        d = d :+ arrLeft(i)
        i += 1
      } else {
        d = d :+ arrRight(j)
        j += 1
        count += arrLeft.length - i
      }
    }
    if (i == arrLeft.length) {
      d = d ++ arrRight.slice(j, arrRight.length)
    } else if (j == arrRight.length) {
      d = d ++ arrLeft.slice(i, arrLeft.length)
    }

    (d, count)
  }


  def SortAndCount(arr: Array[Int]): (Array[Int], Long) = {
    if (arr.length == 1) (arr, 0)
    else {
      val middle = arr.length / 2

      val arr_x = SortAndCount(arr.slice(0, middle))
      val arr_y = SortAndCount(arr.slice(middle, arr.length))
      val arr_z = CountSplitInv(arr_x._1, arr_y._1)

      val x = arr_x._2
      val y = arr_y._2
      val z = arr_z._2

      (arr_z._1, x + y + z)
    }
  }

  def getString(array: Array[Int]): String = {
    return "[" + array.mkString(", ") + "]"
  }

  def main(args: Array[String]): Unit = {
    val intArr = Array(5, 4, 3, 2, 1)
    val intArr1 = Array(1, 3, 5, 2, 4, 6)
    val a = Array(1, 6, 3, 2, 4, 5)
    val b = Array(9, 12, 3, 1, 6, 8, 2, 5, 14, 13, 11, 7, 10, 4, 0)
    val c = Array(1, 5, 3, 2, 4)
    val d = Array(37, 7, 2, 14, 35, 47, 10, 24, 44, 17, 34, 11, 16, 48, 1, 39, 6, 33, 43, 26, 40, 4, 28, 5, 38, 41, 42, 12, 13, 21, 29, 18, 3, 19, 0, 32, 46, 27, 31, 25, 15, 36, 20, 8, 9, 49, 22, 23, 30, 45)
    val e = Array(4, 80, 70, 23, 9, 60, 68, 27, 66, 78, 12, 40, 52, 53, 44, 8, 49, 28, 18, 46, 21, 39, 51, 7, 87, 99, 69, 62, 84, 6, 79, 67, 14, 98, 83, 0, 96, 5, 82, 10, 26, 48, 3, 2, 15, 92, 11, 55, 63, 97, 43, 45, 81, 42, 95, 20, 25, 74, 24, 72, 91, 35, 86, 19, 75, 58, 71, 47, 76, 59, 64, 93, 17, 50, 56, 94, 90, 89, 32, 37, 34, 65, 1, 73, 41, 36, 57, 77, 30, 22, 13, 29, 38, 16, 88, 61, 31, 85, 33, 54)

    //    println(result._1.mkString(", "))
    //    println(result._2)
    //    println(getString(SortAndCount((test))._1))
    //    println(SortAndCount((e))._2)
    //
    val filename = "/Users/caoquan/Desktop/text.txt"
    var inputArray: Array[Int] = Array[Int]()
    for (line <- Source.fromFile(filename).getLines()) {
      inputArray = inputArray :+ line.toInt
    }
    println(inputArray.length)
    println(SortAndCount(inputArray))

  }
}
