import scala.io.Source
import scala.collection.mutable 

object MergeSort {
  val usage = """
    Usage: MergeSort.scala [filename]
  """

  var inversions: Long = 0

  def main (args: Array[String]) {
    if (args.length != 1) {
      println(usage)
      return
    }
    val filename =  args.toList(0)
    val lines = Source.fromFile(filename).getLines().map(_.toInt).toBuffer
    //println("original array: " + lines)
    mergeSort(lines, 0, lines.length - 1)
    println("number of inversions: " + inversions)
  }

  def mergeSort (list: mutable.Buffer[Int], p: Int, r: Int): Any = {
    if (r > p) {
      var q: Int = (p + r) / 2
      //println("Middle point: " + q)
      mergeSort(list, p, q)
      mergeSort(list, q + 1, r)
      merge(list, p, q, r) 
    }
  }

  def merge (list: mutable.Buffer[Int], p: Int, q: Int, r: Int): Any = {
    //Create intermediate arrays
    val lowHalf: mutable.ListBuffer[Int] = mutable.ListBuffer()
    val highHalf: mutable.ListBuffer[Int] = mutable.ListBuffer()
    var k: Int = p

    (k to q).foreach { (e: Int) => lowHalf += list(e); }
    ((q + 1) to r).foreach { (e: Int) => highHalf += list(e) }
    //println("Low half array: " + lowHalf)
    //println("High half array: " + highHalf)
 
    var i: Int = 0
    var j: Int = 0

    //sort the sub array
    while ((i: Int) <= (q - p) && (j: Int) <= (r - q - 1)) {
      if (lowHalf(i) < highHalf(j)) {
        list(k) = lowHalf(i)
        k += 1
        i += 1
      } else if (lowHalf(i) > highHalf(j)) {
        list(k) = highHalf(j)
        k += 1
        j += 1
        //add inversions for the current j element
        inversions += 1 + (q - p) - i 
      } else {
        list(k) = highHalf(j)
        k += 1
        j += 1
        i += 1
      }
    }

    //println("Sorted subarray: " + list)
    
    //now concatenate leftovers
    while (i <= (q - p)) {
      list(k) = lowHalf(i)
      i += 1
      k += 1
    }

    while (j <= (r - q - 1)) {
      list(k) = highHalf(j)
      j += 1
      k += 1
    }

    //println("Concatenated subarray: " + list)
  }
}
