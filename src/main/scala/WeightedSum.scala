import scala.io.Source
import scala.util.Sorting

/**
  * Created by caoquan on 11/21/16.
  */
object WeightedSum {

  class Job(inputWeight: Int, inputLength: Int) extends Ordered[Job] {
    var weight: Int = inputWeight
    var length: Int = inputLength
    var diff: Int = weight - length
    var completionTime: Int = 0


    def compare(that: Job) =
      if (that.diff == this.diff)
        that.weight - this.weight
      else
        that.diff - this.diff

    override def toString: String =
      "(weight: " + weight + ", length: " + length + ", diff: " + diff + ", completion time: " + completionTime + ")"
  }

  def main(args: Array[String]): Unit = {

    // Read data from the file
    println("Input: ")
    val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/jobs.txt"
    //        val filename = "/Users/caoquan/IdeaProjects/coursera-algo/data/jobstest.txt"
    // put data into a hash table
    val iter = Source.fromFile(filename).getLines()
    val firstLine = iter.next()
    val numJobs = firstLine.toInt

    var jobs: Array[Job] = Array()

    for (line <- iter) {
      val strArr = line.split(" ")
      val job = new Job(strArr(0).toInt, strArr(1).toInt)
      jobs = jobs :+ job
    }

    Sorting.quickSort(jobs)

    var weightedSum: Long = 0
    for (i <- 0 to (numJobs - 1)) {
      for (j <- 0 to i)
        jobs(i).completionTime += jobs(j).length
      println(jobs(i))
    }
    for (job <- jobs) {
      weightedSum += job.completionTime * job.weight
    }

    println(weightedSum) //69119377652
  }
}
