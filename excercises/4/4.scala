import annotation.tailrec

import Option._
import scala.util.Try

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

object Option {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      ra <- a
      rb <- b
    } yield f(ra, rb)

  def sequence[A](as: Seq[Option[A]]): Option[Seq[A]] =
    Try(as.map(_.get)).toOption

}

object Main {

  def main(args: Array[String]): Unit = {

    val x = List(Some(1), Some(2), Some(3))
    val y = List(Some(1), None, Some(3))

    println(sequence(x))
    println(sequence(y))

  }

}
