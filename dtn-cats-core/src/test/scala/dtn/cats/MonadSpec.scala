package dtn.cats

import dtn.cats.Monad.{OptionT, listMonad, optMonad, optionTMonad}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MonadSpec extends AnyFunSuite with Matchers:
  test("ifM"):
    given Monad[Option] = optMonad
    given Monad[List] = listMonad
    
    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be (Option("truthy"))
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be (List(1, 2, 3, 4, 1, 2))
    
  test("OptionT"):
    given Monad[List] = listMonad
    
    optionTMonad[List].pure(42) should be(OptionT(List(Some(42))))