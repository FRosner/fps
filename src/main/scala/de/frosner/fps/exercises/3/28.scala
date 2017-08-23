import annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Main {

  def main(args: Array[String]): Unit = {

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
      tree match {
        case Leaf(a) => f(a)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      }

    def size[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)(_ + _)

    def maximum(tree: Tree[Int]): Int =
      fold(tree)(identity)(_ max _)

    def depth[A](t: Tree[A]): Int =
      fold(t)(_ => 0)((l, r) => (l + 1) max (r + 1))

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t){
        a => Leaf(f(a)): Tree[B]
      }{
        (l, r) => Branch(l, r): Tree[B]
      }

    val t1: Tree[Int] = Leaf(5)
    val t2: Tree[Int] = Branch(Leaf(1), Leaf(2))
    val t3: Tree[Int] = Branch(t2, t1)
    println(t3)
    println(map(t3)(_ + 1))

  }

}
