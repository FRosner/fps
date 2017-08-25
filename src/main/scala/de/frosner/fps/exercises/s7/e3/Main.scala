package de.frosner.fps.exercises.s7.e3

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

import de.frosner.fps.exercises.s7.e3.Par.Par

import scala.concurrent.duration.TimeUnit

//class ExecutorService {
//  def submit[A](a: Callable[A]): Future[A] = ???
//}
//
//trait Callable[A] { def call: A }
//trait Future[A] {
//  def get: A
//  def get(timeout: Long, unit: TimeUnit): A
//  def cancel(evenIfRunning: Boolean): Boolean
//  def isDone: Boolean
//  def isCancelled: Boolean
//}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = a(Executors.newFixedThreadPool(10)).get()
}


object Main extends App {

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    val intsLength = ints.length
    if (intsLength <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(intsLength / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  println(Par.run(sum(0 to 10)))

}
