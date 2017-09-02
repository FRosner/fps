package de.frosner.fps.exercises.s7.e5

import java.util.concurrent._

import de.frosner.fps.exercises.s7.e3.Par.Par

import scala.concurrent.duration.{Duration, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B])(
      f: (A, B) => C)
      extends Future[C] {
    def isDone = a.isDone && b.isDone
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = f(a.get, b.get)
    def get(timeout: Long, units: TimeUnit) = {
      val timeoutDuration = Duration.create(timeout, units)
      val (gotA, aTimeNanos) = {
        val aStart = System.nanoTime()
        val gotA = a.get(timeout, units)
        val aEnd = System.nanoTime()
        (gotA, aEnd - aStart)
      }
      val gotB =
        b.get(timeoutDuration.toNanos - aTimeNanos, TimeUnit.NANOSECONDS)
      f(gotA, gotB)
    }
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf)(f)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, Par.unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call: A = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A =
    a(Executors.newFixedThreadPool(10)).get(2, TimeUnit.SECONDS)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => Par.lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(Par.unit(List.empty[A])) { (pa, pl) =>
      Par.map2(pl, pa) { (l, a) =>
        a :: l
      }
    }

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

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    Par.map(parList)(_.sorted)

  def test(s: String): String = {
    println(s"${Thread.currentThread().getName} $s")
    s
  }

  println(Par.run(Par.sequence(List(1, 2, 3).map(Par.unit))))
  println(Par.run(Par.parMap(List("a", "b", "c"))(test)))

}
