package chapter3
import scala.math.Numeric

class Exercises {
  import List._

  // Exercise 1
  lazy val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 2
  // Implement the function tail for removing the first element of a List.
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3
  // Implement the function setHead for replacing the first element of a List with a different value.
  def setHead[A](l: List[A])(head: A) = l match {
    case Nil => List(head)
    case Cons(_, xs) => Cons(head, xs)
  }

  // Exercise 4
  // Generalize tail to the function drop, which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => return Nil
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case lst if n == 0 => return lst
  }

  // Exercise 5
  // Implement dropWhile, which takes a predicate
  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Nil => return Nil
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => return lst
  }

  // Exercise 6
  // Implement init, that returns a List consisting of all but the last element of a List
  def init[A](lst: List[A]): List[A] = lst match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // Exercise 7
  // Can product, implemented using foldRight, immediately halt the recursion
  // and return 0.0 if it encounters a 0.0?
  def product3[A](lst: List[A])(implicit num: Numeric[A]): A =
    foldRight(lst, num.zero) { (x, acc) =>
      import num._
      println(s"Iterating over element ${x} to find product.")
      if (x == num.zero)
        return acc
      else
        acc * x
    }

  // Exercise 8
  def exercise8() =
    foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))

  // Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, Nil) => f(z, x)
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 11
  def sum2[A](l: List[A])(implicit num: Numeric[A]): A = {
    import num._
    def go(l: List[A], total: A): A = l match {
      case Nil => return total
      case Cons(x, xs) => go(xs, total + x)
    }
    go(l, num.zero)
  }

  def product2[A](l: List[A])(implicit num: Numeric[A]): A = {
    import num._
    def go(l: List[A], total: A): A = l match {
      case Nil => return total
      case Cons(x, xs) => go(xs, total * x)
    }
    go(l, num.one)
  }

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => append(Cons(x, Nil), acc))

  // Exercise 12.5
  def reverse2[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => append(acc, Cons(x, Nil)))

  // Exercise 13
  // Write foldLeft in terms of foldRight
  // foldRight in terms of foldLeft
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((x, acc) => f(acc, x))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, z)((acc, x) => f(x, acc))

  // Exercise 14
  // Write append in terms of foldLeft
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l2, l1)((acc, x) => Cons(x, acc))

  // Exercise 15 (hard)
  // Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists.
  // Try to use functions we have already defined.
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => append(acc, x))

  // Exercise 16:
  // Write a function that transforms a list of integers by adding 1 to each element.
  def addOne[A](lst: List[A])(implicit num: Numeric[A]) = {
    import num._
    def go(lst: List[A], acc: List[A]): List[A] = lst match {
      case Nil => acc
      case Cons(x, xs) => go(tail(lst), append(acc, Cons(x + num.one, Nil)))
    }
    go(lst, Nil)
  }

  // Exercise 17
  // Write a function that turns each value in a List[Double] into a String.
  def exercise17(lst: List[_]): List[String] =
    foldLeft(lst, Nil: List[String])((acc, x) => append(acc, List(x.toString)))

  // Exercise 18
  def map[A, B](lst: List[A])(f: A => B): List[B] =
    foldLeft(lst, Nil: List[B])((acc, x) => append(List(f(x)), acc))

  // Exercise 19
  def filter[A](lst: List[A])(f: A => Boolean): List[A] =
    foldLeft(lst, Nil: List[A])((acc, x) => if (f(x)) append(List(x), acc) else acc)

  // Exercise 20
  def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] =
    foldLeft(lst, Nil: List[B])((acc, x) => append(acc, f(x)))

  // Exercise 21
  def filter2[A](lst: List[A])(f: A => Boolean): List[A] =
    flatMap(lst)(x => if (f(x)) List(x) else Nil)

  // Exercise 22
  def crossAdd[A](l1: List[A], l2: List[A])(implicit num: Numeric[A]): List[A] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => return Nil
    case (Cons(x, xs), Cons(y, ys)) =>
      import num._
      append(List(x + y), crossAdd(xs, ys))
  }

  // Exercise 23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => return Nil
    case (Cons(x, xs), Cons(y, ys)) =>
      append(List(f(x, y)), zipWith(xs, ys)(f))
  }

  // Exercise 24
  def hasSubsequence[A](lst: List[A], needle: List[A]): Boolean = needle match {
    case Nil => true
    case Cons(n, ns) => lst match {
      case Nil => false
      case Cons(x, xs) =>
        if (n == x)
          hasSubsequence(xs, ns)
        else
          hasSubsequence(xs, needle)
    }
  }

  // Exercise 25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def treeSize[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => treeSize(l) + treeSize(r)
  }

  // Exercise 26
  def max[A](t: Tree[A])(implicit num: Numeric[A]): A = t match {
    case Leaf(x) => x
    case Branch(Leaf(x), Leaf(y)) => num.max(x, y)
    case Branch(Leaf(x), b: Branch[A]) => num.max(max(b), x)
    case Branch(b: Branch[A], Leaf(x)) => num.max(max(b), x)
    case Branch(b1: Branch[A], b2: Branch[A]) => num.max(max(b1), max(b2))
  }

  // Exercise 27
  def depth[A](t: Tree[A])(implicit num: Numeric[A]): Int = {
    import num._
    def go(tree: Tree[A], acc: Int): Int = tree match {
      case Leaf(_) => return acc
      case Branch(x, y) => (depth(x) max depth(y)) + 1
    }
    go(t, 0)
  }

  // Exercise 28
  def treeMap[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
  }

  // Exercise 29
  def fold[A, B](t: Tree[A], z: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(z, a)
    case Branch(Leaf(x), Leaf(y)) => f(f(z, x), y)
    case Branch(l, r) => fold(r, fold(l, z)(f))(f)
  }
}

object Exercises extends Exercises {
  def main(args: Array[String]): Unit = {
    println(s"Test")
    println(s"Exercise 1: ${x == 3}")
    println(s"Exercise 2: ${tail(List(1,2,3)) == List(2,3)}")
    println(s"Exercise 3: ${setHead(List(1,2,3))(4) == List(4,2,3)}")
    println(s"Exercise 4: ${drop(List(1,2,3), 2) == List(3)}")
    println(s"Exercise 5: ${dropWhile(List(1,2,3,4))(_ < 2) == List(2,3,4)}")
    println(s"Exercise 6: ${init(List(1,2,3)) == List(1,2)}")
    println(s"Exercise 7: ${product3(List(1,2,3,0,4,5)) == 0}")
    println(s"Exercise 8: ${exercise8()}")
    println(s"Exercise 9: ${length(List(1,2,3,4)) == 4}")
    println(s"Exercise 10: ${foldLeft(List(1,2,3), 0)(_ + _) == 6}")
    println(s"Exercise 11: ${sum2(List(1,2,3,4)) == 10}")
    println(s"Exercise 11: ${product2(List(1,2,3,4)) == 24}")
    println(s"Exercise 11: ${length2(List(1,2)) == 2}")
    println(s"Exercise 12: ${reverse(List(1,2,3)) == List(3,2,1)}")
    println(s"Exercise 12.5: ${reverse2(List(1,2,3)) == List(3,2,1)}")
    println(s"Exercise 14: ${length(append2(List(1,2,3), List(4,5,6))) == 6}")
    println(s"Exercise 15: ${flatten(List(List(1,2), List(3,4))) == List(1,2,3,4)}")
    println(s"Exercise 16: ${addOne(List(1,2,3)) == List(2,3,4)}")
    println(s"""Exercise 17: ${exercise17(List(1,2,3)) == List("1", "2", "3")}""")
    println(s"Exercise 18: ${map(List(1,2,3))(_.toString.length) == List(1,1,1)}")
    println(s"Exercise 19: ${filter(List(1,2,3))(_ % 2 == 0) == List(2)}")
    println(s"Exercise 20: ${flatMap(List(1,2,3))(List(_)) == List(1,2,3)}")
    println(s"Exercise 21: ${filter2(List(1,2,3))(_ % 2 == 0) == List(2)}")
    println(s"Exercise 22: ${crossAdd(List(1,2), List(3,4)) == List(4,6)}")
    println(s"Exercise 23: ${zipWith(List(1,2), List(3,4))(_ + _) == List(4,6)}")
    println(s"Exercise 24.1: ${hasSubsequence(List(1,2,3), List(2)) == true}")
    println(s"Exercise 24.2: ${hasSubsequence(List(1,2,3), List(4)) == false}")
    println(s"Exercise 25: ${treeSize(Branch(Leaf(1), Branch(Leaf(1), Leaf(3)))) == 3}")
    println(s"Exercise 26: ${max(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4}")
    println(s"Exercise 27: ${depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(2)), Leaf(2)))) == 3}")
    println(s"Exercise 27.1: ${depth(Leaf(1)) == 0}")
    println(s"Exercise 28: ${treeMap(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1) == Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))}")
    println(s"Exercise 29: ${sizeT(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) == 2}")
  }
}
