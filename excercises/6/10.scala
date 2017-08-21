trait Rng {

  def nextInt: (Int, Rng)

}

case class SimpleRng(seed: Long) extends Rng {

  def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

}


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(
    (s: S) => {
      val (a, s2) = run(s)
      g(a).run(s2)
    }
  )
}

object State {
  // type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) => f.map2(acc)(_ :: _))
}

object Main extends App {

  type Rand[+A] = State[Rng, A]

  def nonNegativeInt: Rand[Int] = State(
    rng => {
      val (i, r) = rng.nextInt
      (if (i < 0) (i + 1) * -1 else i, r)
    }
  )

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - i % 2)

  def double: Rand[Double] = {
    int.map(x => (if (x > 0) (x * -1).toDouble else x) / Int.MinValue.toDouble)
  }

  val int: Rand[Int] = State(_.nextInt)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        State.unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def unfold[A, B](z: B)(f: B => Option[(A, B)]): List[A] = f(z) match {
    case Some((a, b)) => a :: unfold(b)(f)
    case None => Nil
  }

  def ints(n: Int): Rand[List[Int]] =
    State.sequence(List.fill(n)(int))

  val rng = SimpleRng(34343L)
  println(State.unit[Rng, Int](5).map(_ + 1).run(rng))
  println(State.sequence(List.fill(5)(int)).run(rng))

  val ns: Rand[List[Int]] = for {
    x <- nonNegativeLessThan(5)
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % 2)
  println(ns.run(rng))

}
