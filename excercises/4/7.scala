import annotation.tailrec

import Either._

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this.flatMap(f.andThen((b: B) => Right(b)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e): Either[E, B]
    }

  def isRight: Boolean =
    this match {
      case Right(_) => true
      case _ => false
    }

  def isLeft: Boolean =
    this match {
      case Left(_) => true
      case _ => false
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

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.map {
      f.andThen((eb: Either[E, B]) => eb.map(e => List(e)))
    }.reduce[Either[E, List[B]]] {
      case (Right(ebs1), Right(ebs2)) => Right(ebs1 ++ ebs2)
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
    }
  }

}

object Main {

  def main(args: Array[String]): Unit = {

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

    val r = Right(5): Either[String, Int]
    val l = Left("error"): Either[String, Int]

    println(sequence(List(r,r,l)))
    println(sequence(List(r,r)))

  }

}
