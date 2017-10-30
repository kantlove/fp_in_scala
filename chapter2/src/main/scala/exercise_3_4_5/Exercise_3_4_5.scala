package chapter2.exercise_3_4_5

object Exercise_3_4_5 {
  /**
    * Partially apply 1 argument to function `f`. In this case, we apply the 1st argument.
    * @param a the first argument of `f`
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  /**
    * Convert a function `f` of 2 arguments into a function of 1 argument.
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b) // or explicitly, (a: A) => ( (b: B) => f(a, b) )
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Compose f(u) and g(v) into f(g(v))
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
