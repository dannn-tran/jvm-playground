package dtn.cats

import dtn.cats.Apply.optApply
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ApplySpec extends AnyFunSuite with Matchers:
  test("ap"):
    given Apply[Option] = optApply

    Apply[Option].ap(Some(_.toString))(Some(1)) should be(Some("1"))
    Apply[Option].ap[Int, Int](Some(_ * 2))(Some(1)) should be(Some(2))
    Apply[Option].ap[Int, Int](Some(_ * 2))(None) should be(None)
    Apply[Option].ap(None)(Some(1)) should be(None)
    Apply[Option].ap(None)(None) should be(None)