import annotation.tailrec

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this.flatMap(f.andThen((b: B) => Right(b)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e): Either[E, B]
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case right: Right[A] => right: Either[EE, B]
      case left: Left[E] => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Main {

  def main(args: Array[String]): Unit = {

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

    val r = Right(5): Either[String, Int]
    val l = Left("error"): Either[String, Int]

    println(r.map(_ + 2))
    println(l.map(_ + 2))
    println("")

    println(r.flatMap(x => Right(x + 2)))
    println(r.flatMap(x => Left("mist")))
    println(l.flatMap(x => Right(x + 2)))
    println("")

    println(r.orElse(Right(2)))
    println(l.orElse(Right(2)))
    println(l.orElse(Left("shit")))
    println("")

    println(r.map2(r)(_ + _))
    println(l.map2(r)(_ + _))

  }

}
