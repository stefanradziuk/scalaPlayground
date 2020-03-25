sealed trait Maybe[A] {

}

final case class Nothing[A]() extends Maybe[A]

final case class Just[A](content: A) extends Maybe[A]
