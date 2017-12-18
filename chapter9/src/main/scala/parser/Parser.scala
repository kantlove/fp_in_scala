package parser

import language.higherKinds // enable feature Higher-kinded Type

object Main {
  def main(args: Array[String]): Unit = {
  }
}

/**
  * Represents operations that a parser can have. Laws that this trait holds:
  *
  * {{{
  *   // Parsing a char `c` using `char` always returns `c`
  *   run (char(c)) (c.toString) = Right(c)
  *
  *   // Parsing a string `s` using `string` always returns `s`
  *   run (string(s)) (s) = Right(s)
  *
  *   // `or` succeeds if either parser succeeds
  *   run ( or(string(s1), string(s2)) ) (s1) = Right(s1)
  *   run ( or(string(s1), string(s2)) ) (s2) = Right(s2)
  * }}}
  *
  * @tparam ParseError error that will be returned by operations defined in this trait.
  * @tparam Parser     a parser implementation.
  */
trait Parsers[ParseError, Parser[_]] {
  self =>
  /**
    * A parser that recognizes a specific character.
    *
    * @param c the character to recognize.
    * @return exactly character `c`.
    */
  def char(c: Char): Parser[Char] =
    exact(c)

  /**
    * A parser that count the number of occurrence of 'c'.
    *
    * @return the number of occurrence.
    */
  def charCount(c: Char): Parser[Int] =
    count(c)

  /**
    * A parser that recognizes 1 or more `c`, fails if there is no `c`.
    *
    * @return the number of occurrence.
    */
  def charCount2(c: Char): Parser[Int]

  /**
    * A parser that recognizes a specific string.
    *
    * @param s the string to recognize.
    * @return exactly string `s`.
    */
  def string(s: String): Parser[String] =
    exact(s)

  /**
    * A parser that recognizes exactly `a`.
    */
  def exact[A](a: A): Parser[A]

  /**
    * A parser that count the number of occurrence of `a`.
    */
  def count[A](a: A): Parser[Int]

  /**
    * Returns the result of the first succeeded parser.
    * This means:
    * - `a | b` is not necessarily equals to `b | a` (i.e. not commutative).
    * - `a | (b | c)` is not necessarily equals to `(a | b) | c` (i.e. not associative).
    *
    * Note that `p1` will be used first before `p2` so the order of
    * the 2 parsers may affect performance in some cases.
    */
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  /**
    * Runs the parser against an input string.
    *
    * @tparam A type of the result of the parser.
    * @return the result returned by the parser `p` if it succeeds. Otherwise, returns an error.
    */
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit class ParserOps[A](p: Parser[A]) {
    /**
      * Alias of `Parser.or`.
      */
    def |(other: Parser[A]): Parser[A] =
      self.or(p, other)
  }

}

trait CreditCard {
  def charge(p: Int)
}

trait PaymentManager {
  def charge(cc: CreditCard, p: Int)
}

case class Charge(card: CreditCard, price: Int) {
  def combine(other: Charge): Charge =
    other.card match {
      case this.card => Charge(card, price + other.price)
      case _ => throw new Exception("Cannot combine charges of different cards")
    }
}

trait Misc[A, B] {
  val f: A => B

  // Returns the sum of `a` and `b`
  def add(a: Int, b: Int): Int
}

case class Coffee(price: Int = 10)

class Cafe {

//  def buyCoffee(cc: CreditCard): Coffee = {
//    val cup = Coffee()
//    cc.charge(cup.price) // side effect
//    cup
//  }

  def buyCoffee(cc: CreditCard, p: PaymentManager): Coffee = {
    val cup = Coffee()
    p.charge(cc, cup.price) // side effect
    cup
  }

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, quantity: Int): (List[Coffee], Charge) = {
    // Purchase all the coffee. List[(Coffee, Charge)]
    val purchases = List.fill(quantity)(buyCoffee(cc))

    // Turn a list of pair -> pair of list. (List[Coffee], List[Charge])
    val (coffees, charges) = purchases.unzip

    // Return the coffee and the total charge.
    (coffees, charges.reduceLeft((c1, c2) => c1.combine(c2)))
  }

  /**
    * Combines all smaller charges of a each credit card into a single charge.
    */
  def collectCharges(charges: List[Charge]): List[Charge] = {
    val eachCard = charges
      .groupBy(_.card) // Map[ CreditCard, List[Charge] ]
      .values // List[ List[Charge] ]

    eachCard.map(_.reduceLeft(_ combine _)).toList
  }

  def isEven(i: Int): Boolean = {
    println(s"checking $i")
    i % 2 == 0
  }

  def add2(i: Int): Int = {
    println(s"$i + 2")
    i + 2
  }

  val l = List(1, 2, 3, 4, 5).view

  l
    .filter(isEven) // nothing happens
    .map(add2) // nothing happens
    .mkString(", ")

  val sb = new StringBuilder("Hello")
  // sb: StringBuilder = Hello

  val sb2 = sb.append(" World")
  // sb2: StringBuilder = Hello World

  val s1 = sb2.toString()
  // s1: String = Hello World

  val s2 = sb2.toString()
  // s2: String = Hello World World ??

  val l2 = 1 :: List(1)
}
