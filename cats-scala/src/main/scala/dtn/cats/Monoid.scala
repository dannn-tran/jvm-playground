package dtn.cats

import dtn.cats.Semigroup.{intAddSg, strConcatSg}


trait Monoid[T] extends Semigroup[T]:
  def empty: T

object Monoid:
  inline def combine[T](x: T)(y: T)(using sg: Semigroup[T]): T = sg.combine(x, y)
  inline def combineAll[T](xs: List[T])(using m: Monoid[T]): T = xs.fold (m.empty) (m.combine)

  val intAddMonoid: Monoid[Int] = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = intAddSg.combine(x, y)
    override def empty = 0
  }

  val strConcatMonoid: Monoid[String] = new Monoid[String] {
    override def combine(x: String, y: String): String = strConcatSg.combine(x, y)
    override def empty: String = ""
  }
