package chapter10.monoid

object Main {
  def main(args: Array[String]): Unit = {
    println(Util.foldLeft(List(1, 2, 3))("Start:")(_ + " " + _.toString))
    println(Util.foldMapBalance(IndexedSeq(1, 2, 3), Monoids.stringConcat)(_.toString))
  }
}

trait Monoid[A] {

  /**
    * An associative binary operator. This satisfies:
    * {{{
    *   op(op(x, y), z) == op(x, op(y, z)) // i.e. associativity
    * }}}
    */
  def op(a1: A, a2: A): A

  /**
    * An identity value for `op`. This satisfies:
    * {{{
    *   op(x, zero) == op(zero, x) == x
    * }}}
    */
  def zero: A
}

object Monoids {
  val stringConcat: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionCombination[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      /**
        * Note that we never say that `op` must be commutative.
        * This is an example of a non-commutative `op`, order matters in this case.
        * {{{
        *   op(x, y) != op(y, x)
        * }}}
        */
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

      override def zero: Option[A] = None
    }

  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

      override def zero: A => A = x => x
    }

  val wordCount: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = {
      (a1, a2) match {
        case (p1: Piece, p2: Piece) => p1 + p2
        case (s: Segment, p: Piece) => Segment(s.left, s.words, s.right + p)
        case (p: Piece, s: Segment) => Segment(p + s.left, s.words, s.right)
        case (s1: Segment, s2: Segment) =>
          val extra = if((s1.right + s2.left).chars.isEmpty) 0 else 1
          Segment(s1.left, s1.words + extra + s2.words, s2.right)
      }
    }

    override def zero: WordCount = Piece.empty
  }

  def product[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(x: (A, B), y: (A, B)): (A, B) =
        (a.op(x._1, y._1), b.op(y._2, y._2))

      override def zero: (A, B) = (a.zero, b.zero)
    }

  def function[A, B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(f1: A => B, f2: A => B): A => B =
        a => b.op(f1(a), f2(a))

      override def zero: A => B = _ => b.zero
    }
}

object Util {
  /**
    * Concatenates all element in the list into 1, from left to right.
    */
  def concatenate[A](l: List[A], m: Monoid[A]): A =
    l.foldLeft(m.zero)(m.op)

  /**
    * Converts each element of list `l` using `f` and fold left using
    * the operator defined in `m`.
    */
  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B =
    l.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def swap[A, B, C](f: (A, B) => C): (B, A) => C =
    (b, a) => f(a, b)

  /**
    * Implements `List.foldLeft` by `foldMap`.
    * The idea here is to compose the functions not simply mapping over values.
    *
    * Let say we have list l = (1, 2, 3)
    * Then the definition of foldLeft over a function t is:
    * {{{
    *   t( t( t(z, 1), 2 ), 3 )
    * }}}
    *
    * Look at the signature of `f`, we need 1 B and 1 A. A must be from `l`.
    * So we must pass each element in `l` to `f`. To do that, we can turn `f` into:
    * {{{
    *   A => (B => B)
    *
    *   // This means a swap follow by currying
    *   f' = (A, B) => B // swap
    *   g = f'.curried = A => (B => B) // currying
    *
    *   // If we are to pass each element into `g`, it would look like this:
    *   folded: B => B = z => g(3)( g(2)( g(1)(z) ) )
    *
    *   // Wait! The transformation (u, v) => v(u(x)) is exactly `endoMonoid.op(u, v)`
    *   // so we can use that as the monoid in `foldMap`
    * }}}
    */
  def foldLeft[A, B](l: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(l, Monoids.endoMonoid[B])(swap(f).curried)(z)

  /**
    * Same as `foldMap` but splits the list into 2 balance parts to process
    * each time. The reason we use `IndexedSeq` here is because it supports
    * efficient `splitAt` operation.
    */
  def foldMapBalance[A, B](seq: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    seq.size match {
      case 0 => m.zero
      case 1 => f(seq.head)
      case n =>
        val pivot = n / 2
        val (left, right) = seq.splitAt(pivot)

        val leftResult = foldMapBalance(left, m)(f)
        val rightResult = foldMapBalance(right, m)(f)
        m.op(leftResult, rightResult)
    }
  }

  /**
    * Returns the number of words in `text`.
    */
  def countWords(text: String, m: Monoid[WordCount]): Int = {
    def convert(c: Char): WordCount = {
      if(c.toString.trim.isEmpty) Segment(Piece.empty, 0, Piece.empty)
      else Piece(c.toString)
    }

    def inspect(p: Piece) = if(p.chars.isEmpty) 0 else 1

    foldMapBalance(text, Monoids.wordCount)(convert) match {
      case p: Piece => inspect(p)
      case s: Segment =>
        val left = inspect(s.left)
        val right = inspect(s.right)
        left + s.words + right
    }
  }
}

/**
  * Here we implement a simple word count utility.
  */
sealed trait WordCount

object Piece {
  val empty = Piece("")
}

/**
  * Represents an incomplete word.
  */
case class Piece(chars: String) extends WordCount {
  def +(other: Piece): Piece =
    Piece(chars + other.chars)
}

/**
  * Represents a segment of the string being parsed.
  * @param left an incomplete word at the left end.
  * @param words number of complete words counted in this segment.
  * @param right an incomplete word at the right end.
  */
case class Segment(left: Piece, words: Int, right: Piece) extends WordCount
