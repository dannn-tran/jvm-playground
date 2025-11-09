package dtn.cats

import dtn.cats.Apply.{listApply, optApply}

trait Applicative[F[_]] extends Apply[F]:
  def pure[A](x: A): F[A]

object Applicative:
  def apply[F[_]](using a: Applicative[F]): Applicative[F] = a

  val optApplicative: Applicative[Option] = new Applicative[Option]:
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = optApply.map(fa)(f)
    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] = optApply.ap(f)(fa)
    override def pure[A](x: A): Option[A] = Option(x)

  val listApplicative: Applicative[List] = new Applicative[List]:
    override def map[A, B](fa: List[A])(f: A => B): List[B] = listApply.map(fa)(f)
    override def ap[A, B](f: List[A => B])(fa: List[A]): List[B] = listApply.ap(f)(fa)
    override def pure[A](x: A): List[A] = List(x)

