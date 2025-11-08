package dtn.cats

import dtn.cats.Semigroup.{intAddSg, mapMergeSg, strConcatSg}


trait Monoid[T] extends Semigroup[T]:
  def empty: T

object Monoid:
  def apply[T](using m: Monoid[T]): Monoid[T] = m
  implicit class MonoidExtensions[T](m: Monoid[T]):
    def combineAll(xs: List[T]): T = xs.fold (m.empty) (m.combine)

  val intAddMonoid: Monoid[Int] = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = intAddSg.combine(x, y)
    override def empty = 0
  }

  val strConcatMonoid: Monoid[String] = new Monoid[String] {
    override def combine(x: String, y: String): String = strConcatSg.combine(x, y)
    override def empty: String = ""
  }

  def mapMergeMonoid[K, V](using Semigroup[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = mapMergeSg.combine(x, y)
    override def empty: Map[K, V] = Map.empty
  }
