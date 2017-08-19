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

  type Rand[+A] = Rng => (A, Rng)

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

  def ints(count: Int)(rng: Rng): (List[Int], Rng) = {
    val intsAndRngs = List.iterate((0, rng), count + 1){ case (i, r) => r.nextInt }.drop(1)
    (intsAndRngs.map { case (i, r) => i }, intsAndRngs.last match { case (i, r) => r })
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def unfold[A, B](z: B)(f: B => Option[(A, B)]): List[A] = f(z) match {
    case Some((a, b)) => a :: unfold(b)(f)
    case None => Nil
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val asAndRngs = unfold((fs, rng)) {
        case ((h :: t), r) =>
          val (newH, newR) = h(r)
          Some(((newH, newR), (t, newR)))
        case _ => None
      }
      val as = asAndRngs.map { case (a, r) => a }
      val lastRng = asAndRngs.last match { case (a, r) => r }
      (as, lastRng)
    }

  val rng = SimpleRng(34343L)
  println(sequence(List(int, int, int))(rng))
  println(List(int(rng), int(int(rng)._2), int(int(int(rng)._2)._2)))

}
