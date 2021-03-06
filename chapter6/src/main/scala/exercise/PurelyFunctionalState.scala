package chapter6

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)

    val (nonNegative, rng2) = Before.nonNegativeInt(rng)
    println(nonNegative)

    val (d, rng3) = Before.double(rng2)
    println(d)

    val (id, rng4) = Before.intDouble(rng3)
    println(id)

    val (di, rng5) = Before.doubleInt(rng4)
    println(di)

    val nonNegativeLessThan = After.nonNegativeLessThan(10)
    println(nonNegativeLessThan(rng5))
  }
}

/**
  * The point taken from this implementation of a random number generator is: if we have the seed,
  * we can reproduce the exact result every time. This means this code is testable.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    /**
      * The implementation here is not important, just pay attention to the state `nextRNG`.
      * (By the way, this algorithm is called Linear Congruential Generator)
      */
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Before {
  /**
    * Returns a random non-negative integer.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (random, rng2) = rng.nextInt
    val nonNegative = if(random < 0) -(random + 1) else random
    (nonNegative, rng2)
  }

  /**
    * Returns a random double from 0 (inclusive) to 1 (exclusive)
    */
  def double(rng: RNG): (Double, RNG) = {
    val (random, rng2) = nonNegativeInt(rng)
    (random / (Int.MaxValue + 1.0), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }
}

object After {
  type Rand[A] = RNG => (A, RNG)

  /**
    * Returns a result of applying `f` to a random value generated by `s`.
    * @param s 's' stands for 'state action'
    * @return
    */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * Returns a result of applying `f` to a random value generated by `s`.
    * @param s 's' stands for 'state action'
    */
  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a)(rng2)
    }

  def double(rng: RNG): Rand[Double] = {
    map(Before.nonNegativeInt)(_ / (Int.MaxValue + 1.0))
  }

  /**
    * Returns a random non-negative int less than `n`.
    *
    * Since the modulo operation alone is bias because Int.MaxValue may not be divisible by `n`.
    * For example, if Int.MaxValue is 10 and `n` is 4 then remainder 1, 2 have higher probability.
    * Therefore, we need to recursively randomize to avoid such remainder.
    *
    * @see https://stackoverflow.com/a/10984975/3778765
    */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(Before.nonNegativeInt) { i =>
      val mod = i % n
      // Avoid extra numbers at the end, near Int.MaxValue
      if(i - mod + (n - 1) >= 0) (mod, _) else nonNegativeLessThan(n)
    }
  }
}