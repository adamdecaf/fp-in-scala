package chapter2

// EXERCISE 2: Implement isSorted, which checks whether an Array[A]
// is sorted according to a given comparison function:
// def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean

object Exercise2 {

  def isSorted[A](as: List[A], gt: (A, A) => Boolean): Boolean = {
    val zipped = as.zip(as.drop(1))
    zipped.forall{ case (x, y) => gt(x, y) }
  }

  def main(args: Array[String]): Unit = {
    println("All results should be true.")
    println(isSorted[Int](List(1,2,3,4), _ < _))
    println(isSorted[Int](List(1,2,3,4), _ > _) == false)
  }
}
