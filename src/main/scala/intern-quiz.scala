package quiz

// my answers for a little intern quiz we do

object Quiz extends App {
  // implement map via foldLeft
  def map[A, B](in: List[A])(f: A => B): List[B] =
    in.foldLeft(List.empty[B])((acc, elm) => acc :+ f(elm))

  println(map(List(1,2,3))(_ + 2))
}
