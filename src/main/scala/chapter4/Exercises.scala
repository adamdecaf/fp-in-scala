package chapter4

object Exercises extends App {
  sealed trait Option[+A] { self =>
    def map[B](f: A => B): Option[B] = self match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      self.map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = self match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      (self.map(Some(_))).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      if (self.map(f) getOrElse false) self else None
  }

  final case class Some[+A](value: A) extends Option[A]
  final case object None extends Option[Nothing]

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) {
      None
    } else {
      val mean = xs.sum / xs.length
      Some((xs map (x => math.pow(x - mean, 2))).sum / xs.length)
    }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // 4.3
  def map2[A,B,C](xa: Option[A], xb: Option[B])(f: (A,B) => C): Option[C] =
    xa flatMap (a => xb map (b => f(a, b)))

  // def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  // def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty))((acc, opt) => map2(acc, opt)(_ :: _))

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)

  // 4.6
  sealed trait Either[+E, +A] { self =>
    def map[B](f: A => B): Either[E, B] = self match {
      case Right(v) => Right(f(v))
      case Left(v) => Left(v)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
      case Right(v) => f(v)
      case Left(v) => Left(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
      case Right(v) => Right(v)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      self flatMap (s => b map(bb => f(s, bb)))
  }

  final case class Left[+E](v: E) extends Either[E, Nothing]
  final case class Right[+A](v: A) extends Either[Nothing, A]

  // 4.7
  def sequenceE[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((acc, e) => acc.map2(e)(_ :: _))

  def traverseE[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequenceE(es map f)

  // 4.8
  case class Person(name: String, age: Int)

  type EitherNel[E, A] = Either[List[E], A]
  def mkPerson(name: String, age: Int): EitherNel[String, Person] = {
    def mkName(name: String) = if (name.trim.isEmpty) Left("Invalid name" :: Nil) else Right(name)
    def mkAge(age: Int) = if (age <= 0) Left("Invalid Name" :: Nil) else Right(age)

    // However, flatMap doesn't work here...
    // It just takes the first. You could use / follow scalaz.ValidationNel for this
    for {
      n <- mkName(name)
      a <- mkAge(age)
    } yield Person(n, a)
  }

  // Tests
  println(s"variance(Seq(1D, 2D, 3D)) = ${variance(Seq(1D, 2D, 3D))}")
  println(s"variance(Seq.empty) = ${variance(Seq.empty)}")
  println(s"map2 = ${map2(Some(1), Some(2))(_ + _)}")
  println(s"map2 = ${map2(None: Option[Int], Some(2))(_ + _)}")
  println(s"sequence = ${sequence(List(Some(1), Some(2)))}")
  println(s"sequence = ${sequence(List(Some(1), None))}")
  println(s"traverse = ${traverse(List(1,2))(Some(_))}")
  println(s"sequenceE = ${sequenceE(List(Right(1), Right(2)))}")
  println(s"sequenceE = ${sequenceE(List(Right(1), Left(5)))}")
  println(s"traverseE = ${traverseE(List(1,2,3))(Right(_))}")
  println(s"""mkPerson = ${mkPerson("", 0)}""")
  println(s"""mkPerson = ${mkPerson("Bob", 21)}""")
}
