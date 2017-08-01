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
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, xs) => xs
      case _ => throw new Exception()
    }

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

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0)
        l
      else
        drop(tail(l), n - 1)

    def take[A](l: List[A], n: Int): List[A] =
      l match {
        case Cons(x, xs) =>
          if (n <= 0)
            Nil
          else
            Cons(x, take(xs, n - 1))
        case Nil =>
          Nil
      }

    def subSeq[A](l: List[A], from: Int, to: Int): List[A] =
      take(drop(l, from), to - from)

    @tailrec
    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
      case Cons(x, xs) if p(x) => dropWhile(xs, p)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case _ => throw new Exception()
    }

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((c, _) => c + 1)

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

    def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
      (as, bs) match {
        case (Nil, y) => y
        case (x, Nil) => x
        case (Cons(a, aTail), Cons(b, bTail)) => Cons(f(a, b), zipWith(aTail, bTail)(f))
      }

    def fill[T](from: T)(inc: T => T)(whileCond: (T, T) => Boolean)(to: T): List[T] = {
      @tailrec
      def loop(iter: List[T], from: T, to: T): List[T] = {
        if (whileCond(from, to))
          loop(Cons(from, iter), inc(from), to)
        else
          iter
      }
      reverse(loop(Nil, from, to))
    }

    def exists[T](l: List[T])(cond: T => Boolean): Boolean =
      foldLeft(l, false)((z, elem) => z || cond(elem))

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      val subLength = length(sub)
      val supLength = length(sup)
      val subSequences = flatMap {
        fill(0)(_ + 1)(_ < _)(supLength)
      }{
        idx =>
          if (idx + subLength <= supLength)
            Cons(subSeq(sup, idx, idx + subLength), Nil)
          else
            Nil
      }
      exists(subSequences)(_ == sub)
    }

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    val m = Cons(-1, Cons(-2, Nil))
    println(hasSubsequence(fill(0)(_ + 1)(_ < _)(5), fill(1)(_ + 1)(_ < _)(4)))
    println(hasSubsequence(l, m))
    println(hasSubsequence(l, l))
    println(hasSubsequence(l, Cons(3, Cons(1, Nil))))
  }

}
