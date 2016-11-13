/**
  * Created by caoquan on 11/12/16.
  */
object KaratsubaMultiplication {
  def len(x: BigInt) = {
    def len_rec(x: BigInt, i: Int = 1): Int =
      if (x < 10) i
      else len_rec(x / 10, i + 1)
    len_rec(x)
  }

  def multi(x: BigInt, y: BigInt): BigInt = {
    if (len(x) == 1 || len(y) == 1) {
      x * y
    }
    else {
      var length = len(y)
      if (len(x) < len(y)) {
        length = len(x)
      }
      if (length % 2 != 0) {
        length -= 1
      }

      val denum = BigInt(Math.pow(10, length / 2).formatted("%1.0f"))

      val a = x / denum
      val c = y / denum
      val b = x % denum
      val d = y % denum

      val ac = multi(a, c)
      val bd = multi(b, d)


      val abcd = multi(a + b, c + d)

      val ad_bc = abcd - ac - bd

      BigInt(Math.pow(10, length).formatted("%1.0f")) * ac + BigInt(Math.pow(10, length / 2).formatted("%1.0f")) * ad_bc + bd
    }
  }


  def main(args: Array[String]): Unit = {

    val x = BigInt("2718281828459045235360287471352662497757247093699959574966967627")
    val y = BigInt("3141592653589793238462643383279502884197169399375105820974944592")

    println(multi(x, y))
    //    println(multi(12345, 6789))
    //    println(multi(168, 156))
  }
}
