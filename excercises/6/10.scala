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

object Main extends App {

  type Rand[+A] = State[Rng, A]

  type State[S, +A] = S => (A, S)
  // case class State[S, +A](run: S => (A, S))

  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (i, r) = rng.nextInt
    (if (i < 0) (i + 1) * -1 else i, r)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double(rng: Rng): (Double, Rng) = {
    map(int)(x => (if (x > 0) (x * -1).toDouble else x) / Int.MinValue.toDouble)(rng)
  }

  def intDouble(rng: Rng): ((Int, Double), Rng) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: Rng): ((Double, Int), Rng) = {
    val (id, rng2) = intDouble(rng)
    (id.swap, rng2)
  }

  def double3(rng: Rng): ((Double, Double, Double), Rng) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // def ints(count: Int)(rng: Rng): (List[Int], Rng) =
    // sequence(List.fill(count)(int))(rng)

  val int: Rand[Int] = _.nextInt

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = s => {
    val (a, s2) = f(s)
    g(a)(s2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def unfold[A, B](z: B)(f: B => Option[(A, B)]): List[A] = f(z) match {
    case Some((a, b)) => a :: unfold(b)(f)
    case None => Nil
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  val rng = SimpleRng(34343L)
  println(map(unit[Rng, Int](5))(_ + 1)(rng))
  println(sequence(List(int, int, int, int))(rng))

}
