package dtn.cats

import dtn.cats.Functor.{listFunctor, optFunctor}

trait Apply[F[_]] extends Functor[F]:
  def ap[A, B](f: F[A => B])(fa: F[A]): F[B]

object Apply:
  def apply[F[_]](using a: Apply[F]): Apply[F] = a
  
  val optApply: Apply[Option] = new Apply[Option]:
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = optFunctor.map(fa)(f)
    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] = f.flatMap(map(fa)(_))
  
  val listApply: Apply[List] = new Apply[List]:
    override def map[A, B](fa: List[A])(f: A => B): List[B] = listFunctor.map(fa)(f)
    override def ap[A, B](f: List[A => B])(fa: List[A]): List[B] = f.flatMap(map(fa)(_))
    
  
    