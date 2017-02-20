import scala.io.Source

/**
  * Created by caoquan on 11/21/16.
  */
object TwoSat {

  class Clause(val x: Int, val y: Int) {
    override def toString: String =
      " ( " + x + " , " + y + " ) "
  }

  def compute(clause: Clause, variables: Array[Boolean]): Boolean = {
    var boolX = true
    var boolY = true

    if (clause.x < 0) {
      val x = -1 * clause.x
      boolX = !variables(x - 1)
    }
    else {
      boolX = variables(clause.x - 1)
    }

    if (clause.y < 0) {
      val y = -1 * clause.y
      boolY = !variables(y - 1)
    } else {
      boolY = variables(clause.y - 1)
    }
    boolX || boolY
  }

  def allClausesSatisfied(clauses: Array[Clause], variables: Array[Boolean]): (Boolean, Clause) = {
    for (clause <- clauses) {
      val isSatisfied = compute(clause, variables)
      if (!isSatisfied) {
        return (false, clause)
      }
    }
    (true, null)
  }

  def randomBooleanArray(n: Int): Array[Boolean] = {
    var list: Array[Boolean] = Array()
    for (i <- 0 until n) {
      if (Math.random() > 0.5) {
        list = list :+ true
      } else {
        list = list :+ false
      }
    }
    list
  }

  def log2 = (x: Int) => Math.log10(x) / Math.log10(2)

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/2sat1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap1.txt"
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/knap_big.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val n: Int = iter.next().toInt

    var clauses: Array[Clause] = Array()
    for (line <- iter) {
      val strArr = line.split(" ")
      var x = strArr(0).toInt
      var y = strArr(1).toInt

      val clause = new Clause(x, y)
      clauses = clauses :+ clause
    }

//    clauses.foreach(println)


    //    variables.foreach(println)
    //    println(compute(clauses.head, variables))
    for (i <- 1 to Math.ceil(log2(n)).toInt) {
      val variables: Array[Boolean] = randomBooleanArray(n)
      for (i <- 1 to (2 * Math.pow(n, 2)).toInt) {
        val satisfied: (Boolean, Clause) = allClausesSatisfied(clauses, variables)
        if (satisfied._1) {
          println("Satisfied")
          System.exit(1)
        } else {
          val unsatisfiedClause = satisfied._2
          var x = unsatisfiedClause.x
          var y = unsatisfiedClause.y
          if (Math.random() > 0.5) {
            if (x < 0) {
              x = -1 * x
            }
            variables(x - 1) = !variables(x - 1)
          } else {
            if (y < 0) {
              y = -1 * y
            }
            variables(y - 1) = !variables(y - 1)
          }
        }
      }
    }
    println("Unsatisfied")
    // 101100
  }
}
