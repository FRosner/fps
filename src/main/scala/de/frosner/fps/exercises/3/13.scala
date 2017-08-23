import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing] {
  override def toString: String = ""
}
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = head + " " + tail.toString
}

object Main {

  def main(args: Array[String]): Unit = {
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      val g = (b: B, a: A) => f(a, b)
      foldLeft(reverse(as), z)(g)
    }

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    println(foldRight(l, Nil: List[Int])((x, xs) => Cons(x, xs)))
    println(foldLeft(l, Nil: List[Int])((xs, x) => Cons(x, xs)))
  }

}
