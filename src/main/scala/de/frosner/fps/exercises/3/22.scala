import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing] {
  override def toString: String = "Nil"
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

    def concat[A](ll: List[List[A]]): List[A] =
      foldLeft(ll, Nil: List[A])((acc, l) => append(acc, l))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      concat {
        map(as)(f)
      }

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

    def zipAndAdd(as: List[Int], bs: List[Int]): List[Int] =
      (as, bs) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (Cons(a, aTail), Cons(b, bTail)) => Cons(a + b, zipAndAdd(aTail, bTail))
      }

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    val m = Cons(-1, Cons(-2, Nil))
    println(zipAndAdd(l, m))
  }

}
