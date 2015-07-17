import scala.io.Source
import util.Random.nextInt
import scala.collection.mutable 

object QuickSort {
  val usage = """
    Usage: MergeSort.scala [filename]
  """

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val lines = Source.fromFile(filename).getLines().map(_.toInt).toBuffer
    println("original array: " + lines)
    quickSort(lines, 0, lines.length - 1)
    println("sorted array: " + lines)
  }

  def quickSort (list: mutable.Buffer[Int], p: Int, r: Int): Any = {
    if (r > p) {
      var pivot: Int = partition(list, p, r) 
      quickSort(list, p, pivot - 1)
      quickSort(list, pivot + 1, r)
    }
  }

  def partition (list: mutable.Buffer[Int], p: Int, r: Int): Int = {
    swap(list, r, nextInt(r - p) + p)
    var q = p
    (p to (r - 1)).foreach { j =>
      if (list(r) >= list(j)) {
        swap(list, j, q)
        q += 1
      }
    }
    swap(list, q, r)
    q
  }

  def swap (list: mutable.Buffer[Int], first: Int, last: Int): Any = {
    val temp = list(first)
    list(first) = list(last)
    list(last) = temp
  }
}
