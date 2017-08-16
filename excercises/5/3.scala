import annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def iter[A](z: A)(step: A => A): Stream[A] =
    cons(z, {println(z); iter(step(z))(step)})

}

object Main {

  def main(args: Array[String]): Unit = {

    val x = Stream(1, 2, 3)
    println(x.toList)
    println()

    val y = Cons(() => {println(1); 1}, () => Cons(() => {println(2); 2}, () => Empty))
    println(y.toList)
    println()

    val z = Stream.iter(0)(_ + 1)
    println(z.take(5).toList)
    println(z.take(10).toList)
    println(z.drop(5).take(5).toList)
    println(z.takeWhile(_ < 20).toList)
    println()

  }

}
