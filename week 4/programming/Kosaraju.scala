import scala.io.Source
import util.Random.nextInt
import scala.collection.{mutable, immutable}
import scala.runtime.ScalaRunTime._
import scala.util.control.Breaks._

object Kosaraju {
  val usage = """
    Usage: scala Kosaraju.scala [filename]
  """

  var t: Int = 0
  var s: Int = 0
  var scc: Int = 0
  var finish_list: mutable.HashMap[Int, Int] = mutable.HashMap[Int,Int]()
  var explored_list: List[Int] = List()
  var leader: mutable.HashMap[Int,mutable.Buffer[Int]] = mutable.HashMap[Int,mutable.Buffer[Int]]()
  var scc_map: mutable.HashMap[Int,mutable.Buffer[Int]] = mutable.HashMap[Int,mutable.Buffer[Int]]()

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val edges: mutable.Buffer[Array[Int]] = 
      Source.fromFile(filename).getLines().map(_.split(" ").map(_.toInt)).toBuffer

    //edges.foreach(e => println(e(0).toString + ' ' + e(1).toString))

    var double_adj_list: mutable.HashMap[Int, mutable.Buffer[mutable.Buffer[Int]]] =
      mutable.HashMap[Int,mutable.Buffer[mutable.Buffer[Int]]]()

    println("Building double adjacency list....")

    edges.foreach { e =>
      if (! double_adj_list.contains(e(0))) {
        double_adj_list +=
          (e(0) -> mutable.Buffer[mutable.Buffer[Int]](mutable.Buffer[Int](e(1)),
                                                       mutable.Buffer[Int]()))
      } else {
        double_adj_list(e(0))(0).append(e(1))
      }

      if (! double_adj_list.contains(e(1))) {
        double_adj_list +=
          (e(1) -> mutable.Buffer[mutable.Buffer[Int]](mutable.Buffer[Int](),
                                                       mutable.Buffer[Int](e(0))))
      } else {
        double_adj_list(e(1))(1).append(e(0))
      }
    }

    println("...Done")

    println("First round of DFS....")

    (1 to double_adj_list.keys.size).reverse.foreach ( e =>
      if (! explored_list.contains(e)) {
        s = e
        leader += (s -> mutable.Buffer[Int](s)) 
        DFS(double_adj_list, e, 1, 0)
      })

    println("...Done")
    println("Second round of DFS....")

    explored_list = List()

    (1 to double_adj_list.keys.size).reverse.foreach ( e =>
      if (! explored_list.contains(finish_list(e))) {
        s = finish_list(e)
        scc += 1
        scc_map += (s -> mutable.Buffer[Int](s))
        DFS(double_adj_list, s, 0, 1)
      })

    println("...Done")

    scc_map.keys.foreach ( k =>
      println(scc_map(k).length))
  }

  def DFS(double_adj_list: mutable.HashMap[Int, mutable.Buffer[mutable.Buffer[Int]]],
          node: Int,
          dir: Int,
          count: Int): Any = {
    explored_list = explored_list :+ node
    if (count == 0 && ! leader(s).contains(node)) {
      leader(s) = leader(s) :+ node
    }
  
    double_adj_list(node)(dir).foreach ( l =>
      if (! explored_list.contains(l)) {
        if (count == 1) {
          scc_map(s) += l
        }
        DFS(double_adj_list, l, dir, count)
      })

    if (count == 0) {
      t += 1
      finish_list += (t -> node)
    }
  }
}
