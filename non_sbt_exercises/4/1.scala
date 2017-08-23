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

    val x = Some(5)
    val y = None: Option[Int]

    println(x.map(_ + 1))
    println(y.map(_ + 1))
    println("")

    println(x.getOrElse(3))
    println(y.getOrElse(3))
    println("")

    println(x.flatMap(v => Some(v)))
    println(x.flatMap(v => None))
    println(y.flatMap(v => Some(v)))
    println("")

    println(x.orElse(Some(2)))
    println(y.orElse(Some(2)))
    println("")

    println(x.filter(_ == 3))
    println("")

  }

}
