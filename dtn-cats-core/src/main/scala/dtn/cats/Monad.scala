package dtn.cats

import dtn.cats.Applicative.{listApplicative, optApplicative}

trait Monad[F[_]] extends Applicative[F]:
  def flatten[A](ff: F[F[A]]): F[A]

object Monad:
  def apply[F[_]](using m: Monad[F]): Monad[F] = m

  implicit class MonadExtensions[F[_]](m: Monad[F]):
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = m.flatten(m.map(fa)(f))
    def ifM[T](fif: F[Boolean])(ifTrue: F[T], ifFalse: F[T]): F[T] = m.flatMap(fif)(if (_) ifTrue else ifFalse)

  val optMonad: Monad[Option] = new Monad[Option]:
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = optApplicative.map(fa)(f)
    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] = optApplicative.ap(f)(fa)
    override def pure[A](x: A): Option[A] = optApplicative.pure(x)
    override def flatten[A](ff: Option[Option[A]]): Option[A] = ff.flatten()

  val listMonad: Monad[List] = new Monad[List]:
    override def map[A, B](fa: List[A])(f: A => B): List[B] = listApplicative.map(fa)(f)
    override def ap[A, B](f: List[A => B])(fa: List[A]): List[B] = listApplicative.ap(f)(fa)
    override def pure[A](x: A): List[A] = listApplicative.pure(x)
    override def flatten[A](ff: List[List[A]]): List[A] = ff.flatten()

  case class OptionT[F[_], A](value: F[Option[A]])
  def optionTMonad[F[_]](using mf: Monad[F]): Monad[[T] =>> OptionT[F, T]] = new Monad[[T] =>> OptionT[F, T]]:
    override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = OptionT(mf.map(fa.value)(_.map(f)))
    override def ap[A, B](f: OptionT[F, A => B])(fa: OptionT[F, A]): OptionT[F, B] = {
      val funTransform: Option[A => B] => Option[A] => Option[B] = optA2b => optA => optA2b.flatMap(optA.map)
      OptionT(mf.ap(mf.map(f.value)(funTransform))(fa.value))
    }
    override def pure[A](x: A): OptionT[F, A] = OptionT(mf.pure(Option(x)))
    override def flatten[A](ff: OptionT[F, OptionT[F, A]]): OptionT[F, A] =
      val fofo = mf.map(ff.value)(_.map(_.value))
      val ofo2fo: Option[F[Option[A]]] => F[Option[A]] = {
        case Some(value) => value
        case None => mf.map(pure[Option[A]](None).value)(_.flatten())
      }
      val ffo = mf.map(fofo)(ofo2fo)
      val fo = mf.flatten(ffo)
      OptionT(fo)

  def eitherMonad[E]: Monad[[T] =>> Either[E, T]] = new Monad[[T] =>> Either[E, T]] {
    override def flatten[A](ff: Either[E, Either[E, A]]): Either[E, A] = ff match {
      case Left(e) => Left(e)
      case Right(Left(e)) => Left(e)
      case Right(Right(a)) => Right(a)
    }
    override def pure[A](x: A): Either[E, A] = Right(x)
    override def ap[A, B](f: Either[E, A => B])(fa: Either[E, A]): Either[E, B] = (f, fa) match {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(f), Right(a)) => Right(f(a))
    }
    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }
