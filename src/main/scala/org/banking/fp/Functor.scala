package org.banking.fp

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A, B](as: F[A])(f: A => B): F[B]
}
