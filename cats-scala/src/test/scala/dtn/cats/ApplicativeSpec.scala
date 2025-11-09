package dtn.cats

import dtn.cats.Applicative.{listApplicative, optApplicative}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ApplicativeSpec extends AnyFunSuite with Matchers:
  test("Option"):
    given Applicative[Option] = optApplicative

    Applicative[Option].pure(1) should be(Option(1))

  test("List"):
    given Applicative[List] = listApplicative

    Applicative[List].pure(1) should be(List(1))