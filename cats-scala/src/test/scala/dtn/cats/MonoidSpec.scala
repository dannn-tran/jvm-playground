package dtn.cats

import dtn.cats.Monoid.{mapMergeMonoid, strConcatMonoid}
import dtn.cats.Semigroup.intAddSg
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MonoidSpec extends AnyFunSuite with Matchers:
  test("String"):
    given Monoid[String] = strConcatMonoid

    Monoid[String].empty should be("")
    Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
    Monoid[String].combineAll(List()) should be("")

  test("Map"):
    given Semigroup[Int] = intAddSg
    given Monoid[Map[String, Int]] = mapMergeMonoid

    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(Map("a" -> 4, "b" -> 2))
    Monoid[Map[String, Int]].combineAll(List()) should be (Map())

  test("FoldMap"):
    val lst = List(1, 2, 3, 4, 5)
