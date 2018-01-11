package chapter5.stream

object Main {
  def main(args: Array[String]): Unit = {

  }
}

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]

/**
  * A stream element. `head` and `tail` only store how to compute its content, not the actually value.
  * 'Cons' = 'Construct'
  */
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]