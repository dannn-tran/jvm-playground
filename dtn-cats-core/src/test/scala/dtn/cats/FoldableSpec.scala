package dtn.cats

import dtn.cats.Applicative.optApplicative
import dtn.cats.Foldable.{listFoldable, optFoldable}
import dtn.cats.Monoid.{intAddMonoid, strConcatMonoid}
import dtn.cats.MonoidK.{listMonoidK, optMonoidK}
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

  test("foldK"):
    given Foldable[List] = listFoldable
    given MonoidK[List] = listMonoidK
    given MonoidK[Option] = optMonoidK

    Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(List(1, 2, 3, 4, 5))
    Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(Option("three"))

  test("find"):
    given Foldable[List] = listFoldable

    Foldable[List].find(List(1, 2, 3))(_ > 2) should be(Some(3))
    Foldable[List].find(List(1, 2, 3))(_ > 5) should be(None)

  test("exists"):
    given Foldable[List] = listFoldable

    Foldable[List].exists(List(1, 2, 3))(_ > 2) should be(true)
    Foldable[List].exists(List(1, 2, 3))(_ > 5) should be(false)

  test("forall"):
    given Foldable[List] = listFoldable

    Foldable[List].forall(List(1, 2, 3))(_ <= 3) should be(true)
    Foldable[List].forall(List(1, 2, 3))(_ < 3) should be(false)

  test("toList"):
    given Foldable[List] = listFoldable
    given Foldable[Option] = optFoldable

    Foldable[List].toList(List(1, 2, 3)) should be(List(1, 2, 3))
    Foldable[Option].toList(Option(42)) should be(List(42))
    Foldable[Option].toList(None) should be(List())

  test("filter_"):
    given Foldable[List] = listFoldable
    given Foldable[Option] = optFoldable

    Foldable[List].filter_(List(1, 2, 3))(_ < 3) should be(List(1, 2))
    Foldable[Option].filter_(Option(42))(_ != 42) should be(List())

  test("traverse_"):
    given Foldable[List] = listFoldable
    given Applicative[Option] = optApplicative

    Foldable[List].traverse_(List("1", "2", "3"))(_.toIntOption) should be(Some(()))
    Foldable[List].traverse_(List("a", "b", "c"))(_.toIntOption) should be(Some(()))
    
  test("compose"):
    given Foldable[List] = listFoldable
    given Foldable[Option] = optFoldable
    given Monoid[Int] = intAddMonoid
    given Monoid[String] = strConcatMonoid
    
    val FoldableListOption = Foldable[List].compose[Option]
    
    FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be(10)
    FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be("123")