import chapter10.monoid.{Monoids, Util}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}

object MonoidLaws extends Properties("MonoidLaws") {
  property("identity") = forAll { (a: String) =>
    val op = Monoids.stringConcat.op _
    val zero = Monoids.stringConcat.zero

    op(a, zero) == a && op(zero, a) == a
  }

  property("associative") = forAll { (a: String, b: String, c: String) =>
    val op = Monoids.stringConcat.op _
    op(op(a, b), c) == op(a, op(b, c))
  }
}

object WordCountLaws extends Properties("WordCountLaws") {
  private val asciiStringGen = Gen.containerOf[Array, Char](Gen.choose[Char](32, 126)).map(_.mkString)

  property("count correctly") = forAll(asciiStringGen) { (text: String) =>
    (text.length <= 10000) ==> {
      val words = text.split("\\s+").count(!_.trim.isEmpty)
      val rs = Util.countWords(text, Monoids.wordCount) == words
      if(!rs) println(s"$text - ours: ${Util.countWords(text, Monoids.wordCount)}, expect: $words")
      rs
    }
  }
}