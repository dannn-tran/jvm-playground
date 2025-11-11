package dtn.cats

import dtn.cats.Foldable.listFoldable
import dtn.cats.Monoid.{intAddMonoid, strConcatMonoid}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FoldableSpec extends AnyFunSuite with Matchers:
  test("foldLeft"):
    given Foldable[List] = listFoldable

    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
    Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) should be("abc")

  test("foldRight"):
    given Foldable[List] = listFoldable

    Foldable[List].foldRight(List(1, 2, 3), Eval.now(0))((x, rest) => Eval.later(x + rest.value)).value should be(6)

  test("fold"):
    given Foldable[List] = listFoldable
    given Monoid[String] = strConcatMonoid
    given Monoid[Int] = intAddMonoid

    Foldable[List].fold(List("a", "b", "c")) should be("abc")
    Foldable[List].fold(List(1, 2, 3)) should be(6)

  test("foldMap"):
    given Foldable[List] = listFoldable
    given Monoid[String] = strConcatMonoid
    given Monoid[Int] = intAddMonoid

    Foldable[List].foldMap(List("a", "b", "c"))(_.length) should be(3)
    Foldable[List].foldMap(List(1, 2, 3))(_.toString) should be("123")