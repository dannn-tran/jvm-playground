package dtn.cats

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EvalSpec extends AnyFunSuite with Matchers:
  test("now"):
    var executed = false

    val eager = Eval.now {
      executed = true
      1 :: 2 :: 3 :: Nil
    }

    executed should be(true)
    eager.value should be(List(1, 2, 3))

  test("later"):
    var executed = 0

    val lazyEval = Eval.later {
      executed += 1
      1 + 2 * 3
    }

    executed should be(0)
    lazyEval.value should be(7)
    executed should be(1)
    lazyEval.value should be(7)
    executed should be(1)

  test("always"):
    val n = 4
    var counter = 0
    val alwaysEval = Eval.always {
      counter = counter + 1
      1 to n
    }

    (1 to n).foreach(_ => alwaysEval.value)
    counter should be(4)
    alwaysEval.value should be(List(1, 2, 3, 4))
    counter should be(5)

  test("defer"):
    val deferredEval = Eval.now(List.fill(3)(0)).flatMap(e => Eval.defer(Eval.later(e)))

    Eval.defer(deferredEval).value should be(List(0, 0, 0))