sealed trait Maybe[A] {
  def fold[B](f: A => B, empty: B): B =
    this match {
      case Nothing() => empty
      case Just(content) => f(content)
    }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = fold[Maybe[B]](f, Nothing[B]())

  //  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
  //    this match {
  //      case Nothing() => Nothing[B]()
  //      case Just(content) => f(content)
  //    }

  def map[B](f: A => B): Maybe[B] = flatMap[B] { x => Just(f(x)) }
}

final case class Nothing[A]() extends Maybe[A]

final case class Just[A](content: A) extends Maybe[A]
