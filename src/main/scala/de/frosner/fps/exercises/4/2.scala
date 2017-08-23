import annotation.tailrec

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(v => Some(v)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(x => if (f(x)) Some(x) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Main {

  def main(args: Array[String]): Unit = {

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      for {
        m <- mean(xs)
        xsNormalized <- Some(xs.map(x => math.pow(x - m, 2)))
        v <- mean(xsNormalized)
      } yield v

    def variance2(xs: Seq[Double]): Option[Double] =
      mean(xs)
        .map(m => xs.map(x => math.pow(x - m, 2)))
        .flatMap(mean)

    val x = Some(5)
    val y = None: Option[Int]

    println(variance2(Seq(1,1,2,1,1)))
    println(variance(Seq.empty[Double]))

  }

}
