package chapter3.exercise

object SinglyLinkedList {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 5)
    println(l)

    /**
      * We can use `==` here correctly thanks to the fact that case class
      * automatically provides a default implementation of `equals`
      */
    assert(List.tail(l) == List(2, 3, 5))

    assert(List.setHead(l, 4) == List(4, 2, 3, 5))

    assert(List.drop(l, 3) == List(5))

    assert(List.dropWhile(l)(_ <= 2) == List(3, 5))

    assert(List.init(l) == List(1, 2, 3))

    assert(List.reverse(l) == List(5, 3, 2, 1))

    assert(List.sum(l) == 11)

    assert(List.product(l) == 30)

    assert(List.length(l) == 4)

    assert(List.foldRight(l, "#")((a, b) => s"$a & $b") == "1 & 2 & 3 & 5 & #")

    assert(List.foldLeft(l, "#")((a, b) => s"$a & $b") == "# & 1 & 2 & 3 & 5")

    assert(List.reverse2(l) == List(5, 3, 2, 1))

    assert(List.foldRight2(l, "#")((a, b) => s"$a & $b") == "1 & 2 & 3 & 5 & #")

    assert(List.append(l, List(-10, 99)) == List(1, 2, 3, 5, -10, 99))

    assert(List.append2(l, List(-10, 99)) == List(1, 2, 3, 5, -10, 99))

    assert(List.concat(List(l, List(0, -6), l)) == List(1, 2, 3, 5, 0, -6, 1, 2, 3, 5))

    assert(List.increase(l, 2) == List(3, 4, 5, 7))

    assert(List.toStringEach(l) == List("1", "2", "3", "5"))

    assert(List.map(l)(x => (x, x.toString)) == List((1, "1"), (2, "2"), (3, "3"), (5, "5")))

    assert(List.filter(l)(x => (x & 1) == 0) == List(2))

    assert(List.flatMap(l)(x => List(x + 0.1, x + 0.2)) == List(1.1, 1.2, 2.1, 2.2, 3.1, 3.2, 5.1, 5.2))

    assert(List.filter2(l)(x => (x & 1) == 0) == List(2))

    assert(List.zipWith(l, List(4, -10, 2, -9, 14, 0))(_ + _) == List(5, -8, 5, -4))

    assert(List.startsWith(l, List(1, 2)))
    assert(!List.startsWith(l, List(1, 2, -3)))
    assert(!List.startsWith(l, List(1, 2, 3, 5, 10)))

    assert(List.hasSubsequence(l, List(5)))
    assert(List.hasSubsequence(l, List(3, 5)))
    assert(!List.hasSubsequence(l, List(1, 3, 5)))
  }
}

sealed trait List[+A]

object Nil extends List[Nothing] {
  override def toString: String = "Nil"
}

case class Elem[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = s"$head, " + tail.toString
}

object List {
  def apply[A](elems: A*): List[A] = {
    if(elems.isEmpty) Nil
    else Elem(elems.head, apply(elems.tail:_*))
  }

  /**
    * Returns a new list without the 1st element.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot drop element from an empty list")
    case Elem(_, tail) => tail
  }

  /**
    * Returns a new list with the 1st element set to `newHead`.
    */
  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Nil
    case Elem(_, tail) => Elem(newHead, tail)
  }

  /**
    * Returns a new list with 1st `n` elements (left to right) dropped.
    * If `n` is larger than the length of the list, all elements will be dropped.
    */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Elem(_, tail) => drop(tail, n - 1)
    }
  }

  /**
    * Drops elements from the list (from left to right) as long as `f` returns true
    * or the list is empty.
    */
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Elem(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  /**
    * Returns a new list consists of all but the last element.
    */

  def init[A](l: List[A]): List[A] = {
    val withoutLast = drop(reverse(l), 1)
    reverse(withoutLast)
  }

  /**
    * Reverse the list.
    */
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], cur: List[A]): List[A] = cur match {
        case Elem(h, t) => loop(Elem(h, acc), t)
        case Nil => acc
    }
    loop(Nil, l)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((cnt, _) => cnt + 1)

  /**
    * Applies an operator `f` to all elements and a start value `z` (from right to left).
    */
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Elem(h, t) => f(h, foldRight(t, z)(f))
    }

  /**
    * Applies an operator `f` to a start value `z` and all elements (from left to right)
    */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Elem(h, t) => foldLeft(t, f(z, h))(f)
    }

  /**
    * Same as `reverse` but implemented using `foldLeft`.
    */
  def reverse2[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((t, h) => Elem(h, t))

  /**
    * Same as `foldRight` but implemented using `foldLeft`.
    * This is important because `foldLeft` is tail-recursive and can be optimized into iterations
    * by the compiler. The old `foldRight` doesn't have this benefit.
    */
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse2(l), z)((a, b) => f(b, a))
  }

  /**
    * Append elements of another list to the end of this list.
    * Note that this method will traverse the whole 1st list.
    */
  def append[A](l: List[A], other: List[A]): List[A] = {
    foldRight2(l, other)(Elem(_, _))
  }

  /**
    * Same as `append` but implemented using `foldLeft`.
    * This implementation is less efficient because it calls `foldLeft` twice.
    */
  def append2[A](l: List[A], other: List[A]): List[A] = {
    foldLeft(reverse(l), other)((t, h) => Elem(h, t))
  }

  /**
    * Concatenate all the lists inside `lists` to form 1 list.
    */
  def concat[A](lists: List[List[A]]): List[A] = {
    foldRight2(lists, Nil : List[A])(append)
  }

  /**
    * Increase each element of the list by `amount`.
    */
  def increase(l: List[Int], amount: Int): List[Int] = {
    foldRight2(l, Nil : List[Int])((h, t) => Elem(h + amount, t))
  }

  /**
    * Convert each element of the list to its string representation.
    */
  def toStringEach[A](l: List[A]): List[String] = {
    foldRight2(l, Nil : List[String])((h, t) => Elem(h.toString, t))
  }

  /**
    * Create a new list by applying `f` to each element.
    */
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight2(l, Nil: List[B])((h, t) => Elem(f(h), t))
  }

  /**
    * Drop elements that don't satisfy the predicate `f`.
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight2(l, Nil: List[A])((h, t) => if(f(h)) Elem(h, t) else t)
  }

  /**
    * Create a new list by concatenating the results of applying `f` to each element.
    */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight2(l, Nil: List[B])((h, t) => concat(List(f(h), t)))
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => if(f(x)) List(x) else Nil)
  }

  /**
    * Create a new list by applying `f` to 2 corresponding elements from 2 provided lists.
    */
  def zipWith[A, B, C](l: List[A], other: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(l1: List[A], l2: List[B], acc: List[C]): List[C] = {
      (l1, l2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Elem(h1, t1), Elem(h2, t2)) => loop(t1, t2, Elem(f(h1, h2), acc))
      }
    }
    // Since in method 'loop' above, we prepend new elements into the result list
    // (for better performance) instead of appending, we have to reverse the result
    // here to make it correct.
    List.reverse(loop(l, other, Nil: List[C]))
  }

  /**
    * Returns `true` if `other` is a sub-sequence of `l` and `false` otherwise.
    * For example, List(1, 2) is a sub-sequence of List(0, 1, 2, 3) but List(1, 4) and List(4) is not.
    */
  def hasSubsequence[A](l: List[A], other: List[A]): Boolean = {
    (l, other) match {
      case (_, Nil) => true
      case (Elem(t1, h1), Elem(t2, _)) =>
        if(t1 == t2 && startsWith(l, other)) true
        else hasSubsequence(h1, other)
      case _ => false
    }
  }

  /**
    * Returns `true` if `other` is a prefix of `l` and `false` otherwise.
    */
  @annotation.tailrec
  def startsWith[A](l: List[A], other: List[A]): Boolean = {
    (l, other) match {
      case (_, Nil) => true
      case (Elem(t1, h1), Elem(t2, h2)) => t1 == t2 && startsWith(h1, h2)
      case _ => false
    }
  }
}
