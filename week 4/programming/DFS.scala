import scala.io.Source
import util.Random.nextInt
import scala.collection.{mutable, immutable}

object DFS {
  val usage = """
    Usage: scala DFS.scala [filename]
  """
 
  var explored_map: mutable.MutableList[Int] = mutable.MutableList()

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val adjacency_list: mutable.Buffer[Array[Int]] = 
      Source.fromFile(filename).getLines().map(_.split(" ").map(_.toInt)).toBuffer

    val graph: List[Array[Int]] = List(Array(1,3,5), Array(2,4), Array(3,1,5), Array(4,2), Array(5,1,3))
    current_label = graph.length

    (1 to graph.length).foreach { e =>
      if (! explored_map.contains(e)) {
          dfs(graph, e)
      }}
  
    println("=====")
    explored_map.foreach(println)
    println("=====")
  }

  def dfs(graph: List[Array[Int]],
          node: Int): Any = {

    explored_map += (node)

    graph(node - 1).drop(1).foreach { l =>
      if (! explored_map.contains(l)) {
        dfs(graph, l)
      }}
  }
}
