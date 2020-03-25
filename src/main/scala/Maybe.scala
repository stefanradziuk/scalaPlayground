sealed trait Maybe[A] {
  def fold[B](f: A => B): Maybe[B] = {
    this match {
      case Nothing() => Nothing()
      case Just(content) => Just[B](f(content))
    }
  }
}

final case class Nothing[A]() extends Maybe[A]

final case class Just[A](content: A) extends Maybe[A]
