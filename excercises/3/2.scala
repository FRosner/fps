sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Main {

  def main(args: Array[String]): Unit = {
    def tail[A](l: Nil[A]): List[A] = l match {
      case Cons(_, xs) => xs
    }

    println(tail(Cons(5, Nil)))
    println(tail(Nil))
  }

}
