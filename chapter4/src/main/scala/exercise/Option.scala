package chapter4.option

object Main {
  def main(args: Array[String]): Unit = {
    val opt = Some(4)
    val n: Option[Int] = None
    println(opt)
    println(n)

    assert(opt.map(_ + 1) == Some(5))
    assert(n.map(_ + 1) == None)

    assert(opt.flatMap(x => Some(x + 1)) == Some(5))
    assert(opt.flatMap(x => None) == None)

    assert(opt.getOrElse(-1) == 4)
    assert(n.getOrElse(9 - 10) == -1)

    assert(opt.orElse(Some(40)) == opt)
    assert(n.orElse(Some(40)) == Some(40))

    assert(opt.filter(_ > 4) == None)
    assert(opt.filter(_ % 2 == 0) == opt)
    assert(n.filter(_ => true) == None)
  }
}

sealed trait Option[+A] {
  /**
    * Returns a new option containing the result of applying `f` to the underlying value if
    * it is available. Otherwise, returns None.
    */
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case None => None
    }
  }

  /**
    * Returns the underlying value if available. Otherwise, returns the result of evaluating `default`.
    */
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case None => default
    }
  }

  /**
    * Returns the result of applying `f` to the underlying value if
    * it is available. Otherwise, returns None.
    */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  /**
    * Same as `flatMap` but implemented using pattern matching.
    */
  def flatMap1[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(v) => f(v)
      case None => None
    }
  }

  /**
    * Returns this option if it has a value. Otherwise, returns the result of evaluating `alternative`.
    */
  def orElse[B >: A](alternative: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse alternative
  }

  /**
    * Same as `orElse` but implemented using pattern matching.
    */
  def orElse1[B >: A](alternative: => Option[B]): Option[B] = {
    this match {
      case Some(_) => this
      case None => alternative
    }
  }

  /**
    * Returns this option if it has a value AND `f` returns `true` when applied to that value.
    * Otherwise, returns None.
    */
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }

  /**
    * Same as `filter` but implemented using pattern matching.
    */
  def filter1(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) => if(f(v)) this else None
      case None => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  /**
    * Convert a function to a new one that operates on Options.
    *
    * The body of this method will be expanded to: (o: Option[A]) => o map f
    *
    * @see https://stackoverflow.com/questions/28375449/meaning-of-underscore-in-lifta-bf-a-b-optiona-optionb-map-f
    */
  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    _ map f
  }

  /**
    * Combines 2 Options using an operator `f`. Returns None if either Option is None.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(va => b.map(vb => f(va, vb)))
  }

  /**
    * Extracts values inside a list of Options. If any element is None, returns None.
    */
  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    if(l.contains(None)) None
    else Some(l.map {case Some(v) => v})
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }
}
