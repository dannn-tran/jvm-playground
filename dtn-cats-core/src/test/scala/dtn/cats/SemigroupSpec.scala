package dtn.cats

import dtn.cats.Semigroup.{SemigroupExtensions, funMergeSg, intAddSg, listConcatSg, mapMergeSg, optLiftSg}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class SemigroupSpec extends AnyFunSuite with Matchers:
  test("Int"):
    given Semigroup[Int] = intAddSg
    
    Semigroup[Int].combine(1, 2) should be(3)

  test("List"):
    given Semigroup[List[Int]] = listConcatSg[Int]
    
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))

  test("Option"):
    given Semigroup[Int] = intAddSg
    given Semigroup[Option[Int]] = optLiftSg[Int]

    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))
    Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))

  test("Fun"):
    given Semigroup[Int] = intAddSg
    given Semigroup[Int => Int] = funMergeSg

    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(67)

  test("Map"):
    given Semigroup[Int] = intAddSg
    given Semigroup[List[Int]] = listConcatSg[Int]
    given [K, V](using Semigroup[V]): Semigroup[Map[K, V]] = mapMergeSg[K, V]

    Map("foo" -> Map("bar" -> 5)) |+| Map("foo" -> Map("bar" -> 6), "baz" -> Map()) should be(Map("foo" -> Map("bar" -> 11), "baz" -> Map()))
    Map("foo" -> List(1, 2)) |+| Map("foo" -> List(3, 4), "bar" -> List(42)) should be(Map("foo" -> List(1, 2, 3, 4), "bar" -> List(42)))
