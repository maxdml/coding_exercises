import scala.io.Source
import util.Random.nextInt
import scala.collection.mutable 

object QuickSort {
  val usage = """
    Usage: MergeSort.scala [filename]
  """
  
  var comparisons: Int = 0

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename = args.toList(0)
    val lines = Source.fromFile(filename).getLines().map(_.toInt).toBuffer
    //val lines= mutable.Buffer(3,4,5,2,1)
    //println("original array: " + lines)
    quickSort(lines, 0, lines.length - 1)
    //println("sorted array: " + lines)
    println("Comparisons: " + comparisons)
  }

  def quickSort (list: mutable.Buffer[Int], p: Int, r: Int): Any = {
    //println(s"QS p: $p, r: $r")
    if (r > p) {
      var pivot: Int = partition(list, p, r)
      //println(s"pivot is $pivot")
      quickSort(list, p, pivot - 1)
      quickSort(list, pivot + 1, r)
    }
  }

  def partition (list: mutable.Buffer[Int], p: Int, r: Int): Int = {
    //swap(list, p, nextInt(r - p) + p)
    //swap(list, p, r)
    val median_of_three: Int = medianOfThree(list, p, (r - p) / 2  + p, r)
    swap(list, p, median_of_three)

    var q: Int = p + 1
    //println(s"q is $q, p is $p and r is $r")
    //println(list)
    //println("=== iterating===")
    ((p + 1) to r).foreach { j =>
      if (list(p) > list(j)) {
        swap(list, j, q)
        q += 1
      }
      //println(list)
    }
    comparisons += (r - p)
    swap(list, p, q - 1)
    //println(list)
    q - 1
  }

  def medianOfThree(list: mutable.Buffer[Int], p: Int, m: Int, r: Int): Int = {
    var (large: Int, small: Int, median: Int) = (0, 0, 0)
    //println(s"picking median between p: $p, m: $m, and r: $r")
  
    if (list(p) > list(m)) {
      large = p
      small = m
    } else {
      large = m
      small = p
    }
    
    if (list(r) > list(large)) {
      median = large
    } else if (list(r) < list(small)) {
      median = small
    } else {
      median = r
    }
    //println(s"Median is $median")
    median
  }

  def swap (list: mutable.Buffer[Int], first: Int, last: Int): Any = {
    val temp = list(first)
    list(first) = list(last)
    list(last) = temp
  }
}
