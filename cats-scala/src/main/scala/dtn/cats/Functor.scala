package dtn.cats

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  def apply[F[_]](using f: Functor[F]): Functor[F] = f
  
  implicit class FunctorExtensions[F[_]](functor: Functor[F]):
    def lift[A, B](fun: A => B): F[A] => F[B] = fa => functor.map(fa)(fun)
    def fproduct[A, B](fa: F[A])(fun: A => B): F[(A, B)] = functor.map(fa)(a => (a, fun(a)))
    def compose[G[_]](g: Functor[G]): Functor[[T] =>> F[G[T]]] = new Functor[[T] =>> F[G[T]]]:
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = functor.map(fa)(g.lift(f))

  val optFunctor: Functor[Option] = new Functor[Option]:
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

  val listFunctor: Functor[List] = new Functor[List]:
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

  def funFunctor[In]: Functor[[T] =>> In => T] = new Functor[[T] =>> In => T]:
    override def map[A, B](fa: In => A)(f: A => B): In => B = fa andThen f