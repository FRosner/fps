import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Main {

  def main(args: Array[String]): Unit = {
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, xs) => xs
    }

    def setHead[A](newHead: A, l: List[A]) = l match {
      case Cons(head, tail) => Cons(newHead, tail)
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0)
        l
      else
        drop(tail(l), n - 1)

    @tailrec
    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
      case Cons(x, xs) if p(x) => dropWhile(xs, p)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, c) => c + 1)

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    println(length(l))
  }

}
