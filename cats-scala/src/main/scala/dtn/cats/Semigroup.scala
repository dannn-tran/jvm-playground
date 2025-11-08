package dtn.cats


trait Semigroup[T]:
  def combine(x: T, y: T): T
  
object Semigroup:
  def apply[T](using sg: Semigroup[T]): Semigroup[T] = sg

  implicit class SemigroupExtension[T](x: T):
    def |+|(y: T)(using sg: Semigroup[T]): T = sg.combine(x, y)

  val intAddSg: Semigroup[Int] =
    (x, y) => x + y

  val strConcatSg: Semigroup[String] =
    (x, y) => x + y

  def listConcatSg[T]: Semigroup[List[T]] =
    (x, y) => x ++ y

  def optLiftSg[T](using Semigroup[T]): Semigroup[Option[T]] =
    (x, y) => (x, y) match {
      case (None, None) => None
      case (x @ Some(_), None) => x
      case (None, y @ Some(_)) => y
      case (Some(x), Some(y)) => Some(x |+| y)
    }

  def funMergeSg[A, B](using Semigroup[B]): Semigroup[A => B] =
    (x, y) => z => x(z) |+| y(z)

  def mapMergeSg[K, V](using Semigroup[V]): Semigroup[Map[K, V]] = {
    val folder = (acc: Map[K, V], y_kv: (K, V)) => acc.updatedWith(y_kv._1)(_.map(_ |+| y_kv._2).orElse(Some(y_kv._2)))
    (x, y) => y.foldLeft(x)(folder)
  }