import annotation.tailrec

import OptionOps._

import scala.util.Try

object OptionOps {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      ra <- a
      rb <- b
    } yield f(ra, rb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    Option(as.map(f).flatten).filter(_.size == as.size)

}

object Main {

  def main(args: Array[String]): Unit = {

    val xs = List(Some(1), Some(2), Some(3))
    val ys = List(Some(1), None, Some(3))
    val zs = List(1,2,3)

    println(sequence(xs))
    println(sequence(ys))
    println(traverse(zs)(z => if (z < 5) Some(z) else None))

  }

}
