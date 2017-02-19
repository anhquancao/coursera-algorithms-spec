object Solution {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var score = new Array[Int](n);
    for(score_i <- 0 to n-1) {
      score(score_i) = sc.nextInt();
    }
    // your code goes here
    var maxScore = score(0)
    var maxTimes = 0

    var minScore = score(0)
    var minTimes = 0

    for (i <- 1 to n-1) {
      println(score(i))
      if (score(i) > maxScore) {
        maxTimes+=1
        maxScore = score(i)
      }
      if (score(i) < minScore) {
        minTimes += 1
        minScore = score(i)
      }
    }
    println(maxScore + " " + minScore)
  }
}
