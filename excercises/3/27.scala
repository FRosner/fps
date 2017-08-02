import annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Main {

  def main(args: Array[String]): Unit = {

    def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B)(g: (B, B) => B): B =
      tree match {
        case Leaf(a) => f(a, z)
        case Branch(left, right) => g(fold(left, z)(f)(g), fold(right, z)(f)(g))
      }

    def size[A](tree: Tree[A]): Int =
      fold(tree, 0)((x, iter) => iter + 1)(_ + _)

    def maximum(tree: Tree[Int]): Int =
      fold(tree, Integer.MIN_VALUE)(_ max _)(_ max _)

    def depth[A](t: Tree[A]): Int =
      fold(t, 0)((a, z) => z)((l, r) => (l + 1) max (r + 1))

    val t1: Tree[Int] = Leaf(5)
    val t2: Tree[Int] = Branch(Leaf(1), Leaf(2))
    val t3: Tree[Int] = Branch(t2, t1)
    println(depth(t1))
    println(depth(t2))
    println(depth(t3))

  }

}
