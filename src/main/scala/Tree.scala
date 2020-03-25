sealed trait Tree[A] {
  def fold[B](end: B)(f: (A, B, B) => B): B = {
    this match {
      case Leaf() => end
      case Node(item, left, right) => f(item, left.fold(end)(f), right.fold(end)(f))
    }
  }

  def map[B](f: A => B): Tree[B] = {
    fold[Tree[B]](Leaf[B]()) {
      (item, left, right) => Node[B](f(item), left.map(f), right.map(f))
    }
  }
}

final case class Leaf[A]() extends Tree[A]

final case class Node[A](item: A, left: Tree[A], right: Tree[A]) extends Tree[A]
