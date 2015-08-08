import scala.io.Source
import util.Random.nextInt
import scala.collection.mutable 
import scala.runtime.ScalaRunTime._
import scala.util.control.Breaks._

object KargerMinCut {
  val usage = """
    Usage: scala KargerMinCut.scala [filename]
  """
 
  var contracted_vertices: Int = 0
  var merged_map: mutable.HashMap[Int,List[Int]] = new mutable.HashMap[Int, List[Int]]()

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val adjacency_list: mutable.Buffer[Array[Int]] = 
      Source.fromFile(filename).getLines().map(_.split(" ").map(_.toInt)).toBuffer

    val original_adj_list = adjacency_list

    //println("initial adjacency_list: " + stringOf(adjacency_list))

    while (contracted_vertices < adjacency_list.length - 2) {
      contract(adjacency_list)
    }

    var cut_a: Array[Int] = Array[Int]()
    var cut_b: Array[Int] = Array[Int]()
    var cross: Int = 0

    /* TODO: find a way to break out of the loop when the
    * cuts are found
    */

    //identify the two cuts
    (0 to adjacency_list.length - 1).foreach { e =>
      if ( cut_a.isEmpty &&
           adjacency_list(e) != adjacency_list(e + 1)) {
        cut_a = adjacency_list(e)
        cut_b = adjacency_list(e + 1)
      }}

    cut_a.foreach { e => 
      original_adj_list(e - 1).foreach { l =>
        if (cut_b.contains(l)) {
          cross += 1
        }}}

    //then count final cut 
    println("Final cut: " + cross)
  }

  def contract (list: mutable.Buffer[Array[Int]]): Any = {
    /* TODO: make sure that the edge is picked
    *        from within the set of non merged edges
    **/
    
    //choose random vertex
    val vertex = nextInt(list.length)
    //choose random edge within the vertex connections
    val r_vertex   = list(vertex)(nextInt(list(vertex).length))

    //println("Picked edge: " + (vertex + 1) + "--" + r_vertex)

    //merge r_vertex and vertex pointed by r_edge
    merge(list, vertex, r_vertex - 1)

    contracted_vertices += 1

    //println("number of contracted vertices: " + contracted_vertices)
  }

  def merge (list: mutable.Buffer[Array[Int]], vertex: Int, r_vertex: Int) {
    //keep track of which nodes have been merged together
    //println("updating track list.... ")
    //println("merged map is: " + stringOf(merged_map))
    //println("vertex is: " + vertex + ", r_vertex is: " + r_vertex)

    if (merged_map.contains(vertex)) {
      merged_map(vertex) :+ r_vertex + 1
    } else {
      merged_map += (vertex -> List(r_vertex + 1))
    }

    /* BUG: r_vertex is not augmented with the new merged vertex */
    if (merged_map.contains(r_vertex)) {
      //println("adding r_vertex to vertex")
      merged_map(r_vertex) :+ vertex + 1
      //println("shenme gui: " + merged_map(r_vertex))
    } else {
      merged_map += (r_vertex -> List(vertex + 1))
    }

    //println("merged map is now: " + stringOf(merged_map))

    //println("merging.... ")
    
    var tmpList: Array[Int] = list(vertex) ++ list(r_vertex)
    tmpList = tmpList.distinct
    
    //now remove from list this previously merged nodes
    if (merged_map.contains(vertex)) {
      merged_map(vertex).foreach { e =>
        tmpList = tmpList diff merged_map(e - 1)}
    }

    if (merged_map.contains(r_vertex)) {
      merged_map(r_vertex).foreach { e =>
        tmpList = tmpList diff merged_map(e - 1)}
    }

    //remove looping vertices
    tmpList = tmpList diff List(vertex + 1, r_vertex + 1)

    list(vertex)   = tmpList
    list(r_vertex) = tmpList 

    //update nodes that have been previously merged with vertex or r_vertex
    merged_map(vertex).foreach { e =>
      list(e - 1) = list(vertex)}

    merged_map(r_vertex).foreach { e =>
      list(e - 1) = list(r_vertex)}

    //println("updated adjacency list is: " + stringOf(list))
  }
}
