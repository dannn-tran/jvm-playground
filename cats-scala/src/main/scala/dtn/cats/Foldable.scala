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
    def foldK[G[_], T](fa: F[G[T]])(using m: MonoidK[G]): G[T] =
      foldable.foldLeft(fa, m.empty)(m.combineK)
    def find[T](ft: F[T])(pred: T => Boolean): Option[T] = foldable.foldLeft(ft, None: Option[T]) {
      case (acc @ Some(_), _) => acc
      case (None, t) => Option.when(pred(t))(t)
    }
    def exists[T](ft: F[T])(pred: T => Boolean): Boolean = foldable.foldLeft(ft, false) {
      case (true, _) => true
      case (false, t) => pred(t)
    }
    def forall[T](ft: F[T])(pred: T => Boolean): Boolean = foldable.foldLeft(ft, true) {
      case (false, _) => false
      case (true, t) => pred(t)
    }
    def toList[T](ft: F[T]): List[T] = foldable.foldRight(ft, Eval.now(Nil: List[T])) {
      (t, acc) => acc.flatMap(lst => Eval.now(t :: lst))
    }.value
    def filter_[T](ft: F[T])(pred: T => Boolean): List[T] = foldable.foldRight(ft, Eval.now(Nil: List[T])) {
      case (t, acc) if pred(t) => acc.flatMap(lst => Eval.now(t :: lst))
      case (_, acc) => acc
    }.value
    def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(using applicative: Applicative[G]): G[Unit] =
      foldable.foldLeft(fa, applicative.pure(()))((acc, a) => {
        f(a)
        acc
      })
    def compose[G[_]](using foldableG: Foldable[G]): Foldable[[T] =>> F[G[T]]] = new Foldable[[T] =>> F[G[T]]] {
      override def foldLeft[A, B](fa: F[G[A]], b: B)(f: (B, A) => B): B =
        foldable.foldLeft(fa, b)((acc, ga) => foldableG.foldLeft(ga, acc)(f))
      override def foldRight[A, B](fa: F[G[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        foldable.foldRight(fa, lb)((ga, acc) => foldableG.foldRight(ga, acc)(f))
    }

  val listFoldable: Foldable[List] = new Foldable[List]:
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

  val optFoldable: Foldable[Option] = new Foldable[Option]:
    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case Some(value) => f(b, value)
      case None => b
    }
    override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Some(value) => f(value, lb)
      case None => lb
    }
    