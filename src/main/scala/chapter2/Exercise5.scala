package chapter2

// EXERCISE 5: Implement the higher-order function that composes two functions.
// def compose[A,B,C](f: B => C, g: A => B): A => C

object Exercise5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
