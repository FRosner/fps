import annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] =
    foldRight(Option.empty[A]) {
      (a, b) => Some(a)
    }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) {
      (a, b) => if (p(a)) Cons(() => a, () => b) else Empty
    }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) {
      (a, b) => Cons(() => f(a), () => b)
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) {
      (a, b) => if (f(a)) Cons(() => a, () => b) else b
    }

  def append[B >: A](as2: Stream[B]): Stream[B] =
    foldRight(as2) {
      (a, b) => Cons(() => a, () => b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B]) {
      (a, b) => f(a).append(b)
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def constant[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def iter[A](z: A)(step: A => A): Stream[A] =
    unfold(z)(s => Some((s, step(s))))

  def from(start: Int): Stream[Int] =
    iter(start)(_ + 1)

  def fibs: Stream[Int] = {
    def window(nm: (Int, Int)): Option[(Int, (Int, Int))] = {
      val (n, m) = nm
      val o = n + m
      Some((o, (m, o)))
    }
    cons(0, cons(1, unfold((0,1))(window)))
  }


}

object Main {

  def main(args: Array[String]): Unit = {

    println(Stream.constant(1).take(10).toList)
    println(Stream.iter(1)(_ + 1).take(10).toList)
    println(Stream.fibs.take(10).toList)

  }

}
