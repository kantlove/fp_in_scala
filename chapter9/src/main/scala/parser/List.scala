package parser.list

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
  }
}

sealed trait List[+A] {
  val head: A
  val tail: List[A]

  def prepend[B >: A](a: B): List[B] = Elem(a, this)

  def append[B >: A](a: B): List[B] =
    this match {
      case Nil => Elem(a, Nil)
      case Elem(h, t) => Elem(h, t.append(a))
    }

  def sum[B >: A](implicit num: Numeric[B]): B =
    this match {
      case Nil => num.zero
      case Elem(h, t) => num.plus(h, t.sum(num))
    }

  /**
    * Returns the result of applying `f` to each element
    * from left to right with the initial value `zero`.
    *
    * This function is tail-recursive and will be optimized
    * into a loop at compile time.
    */
  @tailrec
  final def foldLeft[B](zero: B)(f: (B, A) => B): B =
    this match {
      case Nil => zero
      case Elem(h, t) => t.foldLeft(f(zero, h))(f)
    }

  def sum2[B >: A](implicit num: Numeric[B]): B =
    foldLeft(num.zero)(num.plus(_, _))

  def reverse: List[A] =
    foldLeft(Nil: List[A])((t, h) => Elem(h, t))

  def foldRight[B](zero: B)(f: (A, B) => B): B =
    reverse.foldLeft(zero)((b, a) => f(a, b))

  def append2[B >: A](a: B): List[B] =
    foldRight(Elem(a, Nil))(Elem(_, _))
}

/**
  * `Nothing` is the subtype of everything so `List[Nothing]`
  * can be the tail of every list.
  */
object Nil extends List[Nothing] {
  override val head: Nothing = ???
  override val tail: List[Nothing] = ???
}

case class Elem[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](a: A*): List[A] = {
    if(a.isEmpty) Nil
    else Elem(a(0), apply(a.drop(1):_*))
  }
}
