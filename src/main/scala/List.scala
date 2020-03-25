sealed trait List[A] {
  def apply(index: Int): A =
    this match {
      case End() => throw new IndexOutOfBoundsException()
      case Cons(head, tail) => index match {
        case 0 => head
        case i => tail(i - 1)
      }
    }

  def fold[B](end: B)(f: (A, B) => B): B =
    this match {
      case End() => end
      case Cons(head, tail) => f(head, tail.fold(end)(f))
    }

  def length: Int = this.fold(0) { (_, sum) => 1 + sum }

  def map[B](f: A => B): List[B] = {
    fold[List[B]](End()) { (head, mappedTail) => Cons(f(head), mappedTail) }
  }

  def contains(item: A): Boolean = fold[Boolean](false) {
    (_: A) == item || (_: Boolean)
  }
}

final case class End[A]() extends List[A]

final case class Cons[A](head: A, tail: List[A]) extends List[A]