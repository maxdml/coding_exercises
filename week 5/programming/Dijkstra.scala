import scala.io.Source
import util.Random.nextInt
import scala.collection.{mutable, immutable}
import scala.runtime.ScalaRunTime._
import scala.util.control.Breaks._

object Dijkstra {
  val usage = """
    Usage: scala BFS.scala [filename]
  """
 
  var queue: Array[Int] = Array[Int](1)

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val adjacency_list: Array[Array[String]] = 
      Source.fromFile(filename).getLines().map(_.split(" ")).toArray

    //println(s"Let's explore this graph!")
    //adjacency_list.foreach( e => //println(e.mkString(", ")))
    //println("=====================================")
    //init map with first node of the graph
    var explored_map: Array[Int] = Array[Int]()
    var dist: mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int](1 -> 0)

    while (! queue.isEmpty) {
      var vertex: Int = queue(0)
      queue = queue.drop(1) 

      //println(s"Exploring state is ${explored_map.mkString}")
      explored_map = explored_map :+ vertex
      //println(s"exploring node ${vertex}")

      var min_dst: mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int]()

      if (! adjacency_list(vertex - 1).drop(1).isEmpty && (explored_map.length != adjacency_list.length)) {
        //run DFS on each node of the explored cut
        explored_map.map { v =>
          val dfs = DFS(adjacency_list, explored_map, dist, v)
          if (dfs._1 > 0) {
            if (min_dst.contains(dfs._1)) {
              if (min_dst(dfs._1) > dfs._2) {
                min_dst(dfs._1) = (dfs._2)
              }
            } else {
              min_dst += (dfs._1 -> dfs._2)
            }
          }
        }
  
        //println(s"min_dst map for node ${vertex} exploration is $min_dst")
        if (dist.contains(min_dst.minBy{ _._2}._1)) {
          if (dist(min_dst.minBy{ _._2}._1) > min_dst.minBy{ _._2}._2) {
            dist(min_dst.minBy{ _._2}._1) = min_dst.minBy{ _._2}._2
          }
        } else {
            dist += (min_dst.minBy{ _._2}._1 -> min_dst.minBy{ _._2}._2)
        }
        queue = queue :+ min_dst.minBy{ _._2}._1
        //println(s"closest node is ${min_dst.minBy{ _._2}._1}, update dist table is $dist")
     }
    }
    println(s"$dist")
  }

  def DFS (adj_list: Array[Array[String]],
           explored_map: Array[Int],
           dist: mutable.HashMap[Int,Int],
           v: Int): (Int, Int) = {

    var min_dst: mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int]()
    var elem: Array[String] = Array[String]()
    var node: Int = 0
    var dst: Int = 0

    adj_list(v - 1).drop(1).foreach { e =>
      elem = e.split(',')
      node = elem(0).toInt
      dst = elem(1).toInt

      if (! explored_map.contains(node)) {
        //println(s"node $v has connection $node which is at distance ${dist(v) + dst} from source")
        min_dst += (node -> (dist(v) + dst))
      }
    }

    if (! min_dst.isEmpty) {
      //println(s"the closest of $v's connection is ${min_dst.minBy{ _._2}._1}, at distance ${min_dst.minBy{ _._2}._2}")
      return (min_dst.minBy{ _._2}._1, min_dst.minBy{ _._2}._2)
    } else {
      return (-1, -1) 
    }
  }
}
