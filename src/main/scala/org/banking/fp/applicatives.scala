package org.banking.fp

import scala.language.higherKinds

object applicatives {

  trait Applicative[F[_]] extends Functor[F]{
    def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

    def apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val a: F[B => C] = map(fa)(f.curried)
      ap(fb)(map(fa)(f.curried))
    }

    def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = apply2(_, _)(f)
  }
}
