object Nnsp {
  def main(args: Array[String]) {
    val p01 = last(List(1, 1, 2, 3, 5, 8))
    assert(p01 == 8)
    println("P01: last(List(1, 1, 2, 3, 5, 8)) => " + p01)

    val p02 = lastButOne(List(1, 1, 2, 3, 5, 8))
    assert(p02 == 5)
    println("P02: lastButOne(List(1, 1, 2, 3, 5, 8)) => " + p02)

    val p03 = kElement(2, List(1, 1, 2, 3, 5, 8))
    assert(p03 == 2)
    println("P03: kElement(List(1, 1, 2, 3, 5, 8)) => " + p03)

    val p04 = length(List(1, 1, 2, 3, 5, 8))
    assert(p04 == 6)
    println("P04: length(List(1, 1, 2, 3, 5, 8)) => " + p04)

    val p05 = isPalindrome(List(1, 2, 3, 2, 1))
    assert(p05 == true)
    println("P05: isPalindrome(List(1, 2, 3, 2, 1) => true")

    val p06 = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    assert(p06 == List(1, 1, 2, 3, 5, 8))
    println("P06: flatten(List(List(1, 1), 2, List(3, List(5, 8)))) => (1, 1, 2, 3, 5, 8)") 

    val p07 = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(p07 ==  List('a, 'b, 'c, 'a, 'd, 'e))
    println("P07: compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) =  List('a, 'b, 'c, 'a, 'd, 'e)") 
    
    val p08 = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(p08 == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    println("P08: pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))")
    
    val p09 = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(p09 == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    println("P08: pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))")
  }

    /* 
    * Find the last element of a list.
    * Example:
    * 
    * scala> last(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 8Find the last element of a list.
    */

    def last(list: List[Int]): Int = {
      list.last 
    }

    /*
    * Find the last but one element of a list.
    * Example:
    * 
    * scala> penultimate(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 5
    */

    def lastButOne(list: List[Int]): Int = {
      list.init.last
    }

    /*
    * Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * 
    * Example:
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */

    def kElement(index: Int, list: List[Int]): Int = {
      list(index)
    }

    /*
    * Find the number of elements of a list.
    * Example:
    * 
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */

    def length(list: List[Int]): Int = {
      list.length
    }

    /*
    * Reverse a list.
    * Example:
    * 
    * scala> reverse(List(1, 1, 2, 3, 5, 8))
    * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    */

    def reverse(list: List[Int]) : List[Int] = {
      list.reverse
    }

    /*
    * Find out whether a list is a palindrome.
    * Example:
    * 
    * scala> isPalindrome(List(1, 2, 3, 2, 1))
    * res0: Boolean = true
    */

    def isPalindrome(list: List[Int]) : Boolean = {
      if (list.length == 1) {
        true
      } else {
        if (list.head == list.last) {
          isPalindrome(list.init.tail)
        } else {
          false
        }
      }
    }

    /*
    * Flatten a nested list structure.
    * Example:
    *
    * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    */

    def reflat(flattened: List[Int], list: List[Any]): List[Int] = {
//      println(s"State of flattened list: $flattened")
//      println(s"State of flist: $list")
      if (list.length == 0) {
        flattened
      } else {
        list.head match {
          case (l: List[Any]) => {
            reflat(reflat(flattened, l).asInstanceOf[List[Int]], list.drop(1))
          }
          case (e: Int)       => reflat(flattened :+ e, list.drop(1))
          case _              => throw new IllegalArgumentException
        }
      }
    }

    def flatten(list: List[Any]): List[Int] = {
      reflat(List[Int](), list)
    }

    /*
    * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    *  Example:
    *  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    *  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    */

    def compress(list: List[Any]): List[Any] = {
      if (list.length == 1) {
        list
      } else {
        if (list.head == list(1)) {
            compress(list.drop(1))
        } else {
          list.head :: compress(list.drop(1))
        }
      }
    }

    /* (**) Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example:
    * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    */

    def pack[A](list: List[A]): List[List[A]] = {
      if (! list.isEmpty) {
          val (packed, next) = list.span { _ == list.head }
          if (next == Nil) {
            /* Why do I have to create a new list?
            *  See example from REPL:
            *  val (v,p) = l.span{_ == l.head}
            *  v: List[Symbol] = List('a, 'a, 'a, 'a)
            *  p: List[Symbol] = List('b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
            *  Where v and p are both lists.
            */
            List(packed)
          } else {
            packed :: pack(next)
          }
//        list.takeWhile(_ == list.head) :: pack(list.dropWhile(_ == list.head))
      } else {
        List(list)
      }
    }

    /* Run-length encoding of a list.
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    * Example:
    * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */

    def encode(list: List[Any]): List[Tuple2[Int, Any]] = {
      val packed = pack(list)
      packed map { l => (l.length, l.head) }
    }
}
