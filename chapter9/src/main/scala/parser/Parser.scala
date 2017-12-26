package parser

import language.higherKinds // enable feature Higher-kinded Type
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object Main {
  def main(args: Array[String]): Unit = {
  }
}

/**
  * Represents operations that a parser can have.
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
  def count[A](a: A): Parser[Int] =
    many(exact(a)).map(_.size)

  /**
    * A parser that recognizes 0 or more patterns of `p`.
    */
  def many[A](p: Parser[A]): Parser[Seq[A]]

  /**
    * Applies `f` to the result of `p`.
    */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

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
      * Alias of `Parsers.or`.
      */
    def |(other: Parser[A]): Parser[A] =
      self.or(p, other)

    /**
      * Alias of `Parsers.map`.
      */
    def map[B](f: A => B): Parser[B] =
      self.map(p)(f)
  }

  /**
    * Laws that this trait holds:
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
    */
  object Laws extends Properties("Parser") {
    /**
      * Parsing a char `c` using `char` always returns `c`
      */
    property("char") = forAll { (c: Char) =>
      run(char(c))(c.toString) == Right(c)
    }
  }
}
