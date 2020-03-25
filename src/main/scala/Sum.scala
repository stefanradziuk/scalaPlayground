sealed trait Sum[A, B] {
  def fold[C](left: A => C, right: B => C): C =
    this match {
      case Failure(a) => left(a)
      case Success(b) => right(b)
    }

  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => f(b)
    }

  def map[C](f: B => C): Sum[A, C] =
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => Success(f(b))
    }
}

final case class Failure[A, B](value: A) extends Sum[A, B]

final case class Success[A, B](value: B) extends Sum[A, B]