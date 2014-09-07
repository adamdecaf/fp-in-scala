package chapter2

// EXERCISE 1:5 Write a recursive function to get the nth Fibonacci
// number ( http://mng.bz/C29s). The first two Fibonacci numbers are
// 0 and 1. The nth number is always the sum of the previous two—the
// sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a local
// tail-recursive function.

// First 10: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34

class FibonacciNumber private(val value: Int, private val position: Int)

object FibonacciNumber {
  def apply(position: Int) =
    new FibonacciNumber(Exercise1.makeFibonacci(position), position)
}

object Exercise1 {
  def makeFibonacci(position: Int): Int = {

    @tailrec
    def go(n: Int, acc: List[Int]): Int =
      if ((acc.size - 1) == position) {
        // println(s"acc = ${acc}, position = ${position}, n = ${n}")
        return acc(position - 1)
      } else {
        // println(s"position = ${position}, n = ${n}, acc = ${acc.toList}")
        go(n + 1, acc :+ (acc(n - 1) + acc(n - 2)))
      }

    go(2, List(0, 1))
  }

  def main(array: Array[String]): Unit = {
    println(FibonacciNumber(5).value)
    println(FibonacciNumber(10).value)
  }
}
