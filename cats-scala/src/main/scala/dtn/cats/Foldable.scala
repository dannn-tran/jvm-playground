package dtn.cats

trait Foldable[F[_]]:
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

object Foldable:
  def apply[F[_]](using f: Foldable[F]): Foldable[F] = f

  implicit class FoldableExtensions[F[_]](foldable: Foldable[F]):
    def fold[T](ft: F[T])(using m: Monoid[T]): T =
      foldable.foldLeft(ft, m.empty)(m.combine)
    def foldMap[A, B](fa: F[A])(f: A => B)(using m: Monoid[B]): B =
      foldable.foldLeft(fa, m.empty)((b, a) => m.combine(b, f(a)))

  val listFoldable: Foldable[List] = new Foldable[List]:
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
