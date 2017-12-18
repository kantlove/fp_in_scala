package chapter7.parallel_library

import java.util.concurrent._

object Main {
  def main(args: Array[String]): Unit = {
    val a = Par.lazyUnit(42 + 1)
    val exe = Executors.newFixedThreadPool(2)
    println(Par.equal(exe)(a, Par.fork(a)))
  }
}

object Par {
  /**
    * Represent a computation that can be executed concurrently.
    *
    * There are 2 ways Par can be evaluated, either immediately
    * through `unit` or concurrently through `fork`.
    */
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  /**
    * Returns a computation that results in value `a`.
    */
  def unit[A](a: A): Par[A] =
    _ => UnitFuture(a)

  /**
    * Marks a computation for concurrent evaluation.
    */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = {
        val p = a
        p(es).get
      }
    })

  /**
    * Applies `f` to the result of `a`.
    */
  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit())((x, _) => f(x))

  /**
    * Applies a binary operator `f` to the result of `a` and `b`.
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
    * Execute the computation.
    */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  /**
    * Wraps the expression `a` for concurrent evaluation by `run`.
    */
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /**
    * Converts `f` to a function that evaluates concurrently.
    */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

//  /**
//    * Returns the computation that will sort its result list.
//    */
//  def sortPar[A](parList: Par[List[A]]): Par[List[A]] = map(parList)(_.sorted)

  /**
    * Converts a list of `Par` to a `Par` that returns a list.
    */
  def sequence[A](lp: List[Par[A]]): Par[List[A]] = {
    lp.foldRight[Par[List[A]]]( unit(List()) )( (h, t) => map2(h, t)(_ :: _) )
  }

  /**
    * Concurrently applies `f` to elements of `l`.
    */
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val listOfPar = l.map(asyncF(f))
    sequence(listOfPar)
  }

  def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(es).get == p2(es).get
}