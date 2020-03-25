sealed trait List[A] {
  def apply(index: Int): A =
    this match {
      case End() => throw new IndexOutOfBoundsException()
      case Cons(head, tail) => index match {
        case 0 => head
        case i => tail(i - 1)
      }
    }

  def append(list: List[A]): List[A] =
    this match {
      case End() => list
      case Cons(head, tail) => Cons(head, tail.append(list))
    }

  def fold[B](end: B)(f: (A, B) => B): B =
    this match {
      case End() => end
      case Cons(head, tail) => f(head, tail.fold(end)(f))
    }

  def map[B](f: A => B): List[B] =
    fold[List[B]](End()) { (head, mappedTail) => Cons(f(head), mappedTail) }

  def flatMap[B](f: A => List[B]): List[B] =
    this match {
      case End() => End()
      case Cons(head, tail) => f(head).append(tail.flatMap[B](f))
    }

  def length: Int = this.fold(0) { (_, sum) => 1 + sum }

  def contains(item: A): Boolean = fold[Boolean](false) {
    (_: A) == item || (_: Boolean)
  }
}

final case class End[A]() extends List[A]

final case class Cons[A](head: A, tail: List[A]) extends List[A]