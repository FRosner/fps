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

    val l = Cons(2, Cons(3, Cons(1, Nil)))
    println(init(l))
  }

}
