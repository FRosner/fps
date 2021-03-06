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

    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)((val1, acc2) => Cons(val1, acc2))

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    val m = Cons(-1, Cons(-2, Nil))
    println(append(l, m))
    println(append(m, l))
  }

}
