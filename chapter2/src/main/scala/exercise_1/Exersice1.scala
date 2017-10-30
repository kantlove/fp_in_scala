package chapter2.exercise_1

object Exersice1 {
  def main(args: Array[String]): Unit = {
    assert(fib(17) == 1597)
    assert(fib(5) == 5)
  }

  /**
    * Compute the n-th (0-based) Fibonacci number.
    */
  def fib(n: Int): Int = {
    /**
      * Inner function to compute the result.
      *
      * `tailrec` annotation is used to tell the compiler to force this function to be in a tail position.
      * This is required for the compiler to optimize the code by eliminating the recursion.
      */
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if(n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }
}
