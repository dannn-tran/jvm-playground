package dtn.cats


trait Semigroup[T]:
  def combine(x: T, y: T): T
  
object Semigroup:
  val intAddSg: Semigroup[Int] = (x: Int, y: Int) => x + y
  given Semigroup[Int] = intAddSg
  
  val strConcatSg: Semigroup[String] = (x: String, y: String) => x + y
  given Semigroup[String] = strConcatSg