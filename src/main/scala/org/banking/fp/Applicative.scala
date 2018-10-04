package org.banking.fp

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F]{
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  def apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    ap(fb)(map(fa)(f.curried))
  }

  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = apply2(_, _)(f)
}

object Applicative {

  implicit def listApplicative: Applicative[List] = new Applicative[List] {

    def ap[A, B](fa: => List[A])(f: => List[A => B]): List[B] = fa zip f map{case (a, func) => func(a)}

    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}
