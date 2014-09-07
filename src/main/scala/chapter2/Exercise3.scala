package chapter2

// EXERCISE 3: Let’s look at another example, currying, which converts a function f
// of two arguments into a function of one argument that partially applies f. Here
// again there’s only one implementation that compiles. Write this implementation.

//Footnote This is named after the mathematician Haskell Curry, who discovered
// the principle. It was independently discovered earlier by Moses Schoenfinkel,
// but Schoenfinkelization didn’t catch on.

// def curry[A,B,C](f: (A, B) => C): A => (B => C)

object Exercise3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a, _)
}
