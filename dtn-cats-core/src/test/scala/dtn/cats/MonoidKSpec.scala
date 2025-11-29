package dtn.cats

import dtn.cats.MonoidK.listMonoidK
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MonoidKSpec extends AnyFunSuite with Matchers:
  test("empty"):
    given MonoidK[List] = listMonoidK

    MonoidK[List].empty[String] should be(List())
    MonoidK[List].empty[Int] should be (List())

  test("combineK"):
    given MonoidK[List] = listMonoidK

    MonoidK[List].combineK(List("hello", "world"), List("goodbye", "moon")) should be(List("hello", "world", "goodbye", "moon"))
    MonoidK[List].combineK(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4))

