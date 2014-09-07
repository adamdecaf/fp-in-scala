package chapter2

// EXERCISE 4: Implement uncurry, which reverses the transformation of curry.
// Note that since => associates to the right, A => (B => C) can be
// written as A => B => C.

// def uncurry[A,B,C](f: A => B => C): (A, B) => C

object Exercise4 {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}
