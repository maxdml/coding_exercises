import scala.io.Source
import util.Random.nextInt
import scala.collection.{mutable, immutable}
import scala.runtime.ScalaRunTime._
import scala.util.control.Breaks._

object BFS {
  val usage = """
    Usage: scala BFS.scala [filename]
  """
 
  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val adjacency_list: mutable.Buffer[Array[Int]] = 
      Source.fromFile(filename).getLines().map(_.split(" ").map(_.toInt)).toBuffer

    val graph: List[Array[Int]] = List(Array(1,2,3), Array(2,1,3,4), Array(3,1,2,4,5), Array(4,2,3), Array(5,3))
    //init queue with first node of the graph
    var queue: mutable.MutableList[Int] = mutable.MutableList(0)
    var explored_map: mutable.MutableList[Int] = mutable.MutableList(0)

    var dist: mutable.HashMap[Int, Int] = mutable.HashMap[Int,Int](1 -> 0)

    while (! queue.isEmpty) {
      val v: Int = queue(0)
      queue = queue.drop(1)

      graph(v).drop(1).foreach { e =>
         if (! explored_map.contains(e - 1)) {
           explored_map += (e - 1)
           queue += (e - 1)
           dist += (e -> (dist(graph(v)(0)) + 1))
        }}
    }

    explored_map.foreach(println)
    println("=====")
    dist.foreach(println)
  }
}
