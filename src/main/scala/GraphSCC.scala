import scala.io.Source
import scala.util.Random
import scala.util.control.Breaks

/**
  * Created by caoquan on 11/21/16.
  */
object GraphSCC {

  var t: Int = 0
  var s: Int = -1
  var explored: Array[Int] = Array()
  var results: Array[Int] = Array()
  var leader: Map[Int, Int] = Map()
  var f: Map[Int, Int] = Map()

  def Kosaraju(G: Array[(Int, Int)], n: Int): Unit = {
    val Grev = reverse(G)

    println("run on reverse")
    DFSLoop(Grev, n)

    var newG: Array[(Int, Int)] = Array()
    for (edge <- G) {
      val l: Int = f.get(edge._1) match {
        case Some(value) => value
      }
      val r: Int = f.get(edge._2) match {
        case Some(value) => value
      }
      newG = newG :+ (l, r)
    }
    println("run on newG")
    DFSLoop(newG, n)
    println(leader.groupBy(_._2).mapValues(_.size).values.toArray.sortWith((e1, e2) => e2 < e1).mkString(", "))
    //    for ((k, v) <- leader) printf("key: %s, value: %s\n", k, v)
  }

  def reverse(G: Array[(Int, Int)]): Array[(Int, Int)] = {
    G.map(item => (item._2, item._1))
  }

  def DFSLoop(G: Array[(Int, Int)], n: Int): Unit = {
    t = 0
    s = -1
    explored = Array()
    results = Array()
    leader = Map()
    f = Map()

    for (i <- n to 1 by -1) {
      if (!explored.contains(i)) {
        s = i
        DFS(G, i)
      }
    }
  }

  def DFS(G: Array[(Int, Int)], i: Int): Unit = {
    println(i)
    explored = explored :+ i
    leader = leader + (i -> s)
    for (arc <- G.filter(item => item._1 == i)) {
      val j = arc._2
      if (!explored.contains(j)) {
        DFS(G, j)
      }
    }
    t += 1
    f = f + (i -> t)
  }

  def main(args: Array[String]): Unit = {
    val test1 = Array(
      (1, 4),
      (2, 8),
      (3, 6),
      (4, 7),
      (5, 2),
      (6, 9),
      (7, 1),
      (8, 5),
      (8, 6),
      (9, 7),
      (9, 3)
    )
    val test2 = Array(
      (1, 2),
      (2, 6),
      (2, 3),
      (2, 4),
      (3, 1),
      (3, 4),
      (4, 5),
      (5, 4),
      (6, 5),
      (6, 7),
      (7, 6),
      (7, 8),
      (8, 5),
      (8, 7)
    )
    val filename = "/Users/caoquan/Desktop/text.txt"
    println("read the file")
    var testFinal: Array[(Int, Int)] = Array()
    for (line <- Source.fromFile(filename).getLines()) {
      val arr = line.split(' ').map((v) => v.toInt)
      val elem = (arr(0), arr(1))
      testFinal = testFinal :+ elem
    }

    //    Kosaraju(test1, 9)
    //    Kosaraju(test2, 8)
    Kosaraju(testFinal, 875714)
  }
}
