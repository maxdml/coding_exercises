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

    val graph: List[Array[Int]] = List(Array(1,3,5), Array(2,4), Array(3,1,5), Array(4,2), Array(5,1,3))
    var explored_map: mutable.MutableList[Int] = mutable.MutableList()
    var connected_comp: mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int]()
    var ncc: Int = 0

    (1 to graph.length).foreach { e =>
      if (! explored_map.contains(e)) {
          ncc += 1
          bfs(graph, e, explored_map, connected_comp, ncc)
      }}
  
    println("=====")
    explored_map.foreach(println)
    println("=====")
    connected_comp.foreach(println)
  }

  def bfs(graph: List[Array[Int]],
          node: Int,
          explored_map: mutable.MutableList[Int],
          connected_comp: mutable.HashMap[Int,Int],
          ncc: Int): Any = {

    var queue: mutable.MutableList[Int] = mutable.MutableList(node - 1)

    connected_comp += (node -> ncc)

    while (! queue.isEmpty) {
      val v: Int = queue(0)
      queue = queue.drop(1)

      graph(v).drop(1).foreach { l =>
        if (! explored_map.contains(l)) {
          connected_comp += (l -> ncc)
          explored_map += (l)
          queue += (l - 1)
        }}
    }
  }
}
