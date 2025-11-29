package dtn.cats

import dtn.cats.Functor.{listFunctor, optFunctor}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FunctorSpec extends AnyFunSuite with Matchers:
  test("map"):
    given Functor[List] = listFunctor
    given Functor[Option] = optFunctor

    Functor[List].map(List("qwer", "adsfg"))(_.length) should be(List(4, 5))
    Functor[Option].map(Option("Hello"))(_.length) should be(Option(5))
    Functor[Option].map(None: Option[String])(_.length) should be(None)

  test("lift"):
    given Functor[Option] = optFunctor
    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)

    lenOption(Some("Hello")) should be(Some(5))

  test("fproduct"):
    given Functor[List] = listFunctor

    Functor[List].fproduct(List("Cats", "is", "awesome"))(_.length) should be(List(("Cats", 4), ("is", 2), ("awesome", 7)))

  test("compose"):
    given Functor[List] = listFunctor
    given Functor[Option] = optFunctor
    val listOpt = Functor[List] compose Functor[Option]
    
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be (List(Some(2), None, Some(4)))