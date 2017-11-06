package chapter3.exercise

object BinaryTree {
  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Leaf(4), Leaf(-10)), Leaf(3)))
    println(tree)

    assert(Tree.maximum(tree) == 4)

    assert(Tree.depth(tree) == 4)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => math.max(maximum(l), maximum(r))
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + math.max(depth(l), depth(r))
    }
  }
}
