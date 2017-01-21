import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object HuffmanCode {

  class Node(var left: Node, var right: Node, var weight: Int) extends Ordered[Node] {
    def compare(that: Node) = this.weight - that.weight

    override def toString: String = "(" + this.weight + ")"
  }

  def removeIndex(list: List[Node], ix: Int) = if (list.size < ix) list else list.take(ix) ++ list.drop(ix + 1)

  def computeLengths(node: Node): Array[String] = {
    var strs: Array[String] = Array()

    def getLength(node: Node, str: String): Unit = {
      if (node.left == null && node.right == null) {
        strs = strs :+ str
        println(node.weight + ": " + str)
      }
      if (node.left != null)
        getLength(node.left, str + "0")
      if (node.right != null)
        getLength(node.right, str + "1")
    }

    getLength(node, "")
    strs
  }


  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    //    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/huffman1.txt"
    //        val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/huffman2.txt"
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/huffman.txt"

    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    var numSymbols: Int = iter.next().toInt

    var nodes: List[Node] = List()
    for (line <- iter) {
      val node = new Node(null, null, line.toInt)
      nodes = node :: nodes
    }
    while (numSymbols > 1) {
      nodes = nodes.sorted
      val newNode = new Node(nodes(0), nodes(1), nodes(0).weight + nodes(1).weight)
      nodes = removeIndex(nodes, 0)
      nodes = removeIndex(nodes, 0)
      nodes = newNode :: nodes
      numSymbols -= 1
//      nodes.foreach(w => print(w + " "))
//      println("\n")
    }
    val lengths = computeLengths(nodes(0))
    val l = lengths.map(e => e.length)
    println(lengths.length)
    println("Max: " + l.max) // 19
    println("Min: " + l.min) // 9
    //    lengths.foreach(l => print(l + " "))
  }
}
