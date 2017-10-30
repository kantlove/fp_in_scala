package chapter2.exercise_2

object Exercise2 {
  def main(args: Array[String]): Unit = {
    val sorted = Array(1, 20, 21, 140)
    val notSorted = Array(99, -10, 6, 15)

    val ascending: (Int, Int) => Boolean = _ < _

    assert(isSorted(sorted)(ascending))
    assert(!isSorted(notSorted)(ascending))
  }

  /**
    * Returns `true` if the array is sorted according to `ordered` and `false` otherwise.
    * @param ordered the ordering condition
    */
  def isSorted[A](arr: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if(i >= arr.length) true
      else if(!ordered(arr(i - 1), arr(i))) false
      else loop(i + 1)
    }

    loop(1)
  }
}
