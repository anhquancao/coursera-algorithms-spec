import scala.io.Source
import scala.util.Random
import scala.util.control.Breaks

/**
  * Created by caoquan on 11/21/16.
  */
object MinCut {

  def computeMinCut(graph: Map[Int, Array[Int]]): Int = {

    var newGraph: Map[Int, Array[Int]] = graph
    while (newGraph.keys.toArray.length > 2) {
      val vertices = newGraph.keys.toArray
      val randVertexIndex = Random.nextInt(vertices.length)
      val randVertex: Int = vertices(randVertexIndex)

      val row = newGraph(randVertex)

      val otherVertex = row(Random.nextInt(row.length))
      val otherRow = newGraph(otherVertex)

      val newRow: Array[Int] = (row ++ otherRow).filter((e: Int) => e != randVertex && e != otherVertex)

      val tempGraph = newGraph.filterKeys((k: Int) => k != randVertex && k != otherVertex)
      newGraph = Map(randVertex -> newRow)
      tempGraph.foreach { case (key, value) =>
        val tempArr = value.map((v: Int) => if (v == otherVertex) randVertex else v)
        newGraph = newGraph + (key -> tempArr)
      }
    }
    val values = newGraph.values.toArray
    values(1).length
  }

  def main(args: Array[String]): Unit = {
    val graph = Map(
      1 -> Array(2, 3, 4, 7),
      2 -> Array(1, 3, 4),
      3 -> Array(1, 2, 4),
      4 -> Array(1, 2, 3, 5),
      5 -> Array(4, 6, 7, 8),
      6 -> Array(5, 7, 8),
      7 -> Array(1, 5, 6, 8),
      8 -> Array(5, 6, 7)
    )
    val graph2 = Map(
      1 -> Array(4, 2, 7, 3),
      2 -> Array(4, 1, 3),
      3 -> Array(1, 2, 4),
      4 -> Array(5, 1, 2, 3),
      5 -> Array(8, 7, 6, 4),
      6 -> Array(8, 5, 7),
      7 -> Array(6, 8, 5, 1),
      8 -> Array(7, 6, 5)
    )
    val graph3 = Map(
      1 -> Array(2, 3, 4),
      2 -> Array(1, 3, 4),
      3 -> Array(1, 2, 4),
      4 -> Array(1, 2, 3, 5),
      5 -> Array(4, 6, 7, 8),
      6 -> Array(5, 7, 8),
      7 -> Array(5, 6, 8),
      8 -> Array(5, 6, 7)
    )
    val filename = "/Users/caoquan/Desktop/graph.txt"
    var graphFinal: Map[Int, Array[Int]] = Map()
    for (line <- Source.fromFile(filename).getLines()) {
      val arr = line.split('\t').map((v) => v.toInt)
      graphFinal = graphFinal + (arr(0) -> arr.slice(1, arr.length))
    }

    var min = 200
    for (i <- 1 until 400) {
      val temp = computeMinCut(graphFinal)
      println(temp)
      if (temp < min) {
        min = temp
      }
    }
    println("min: " + min)

  }
}
