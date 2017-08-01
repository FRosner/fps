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

    println(setHead(3, Cons(5, Nil)))
  }

}
