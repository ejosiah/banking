package org.banking.fp

import scala.language.higherKinds

trait Foldable[F[_]] {
  def fold1[A, B](as: F[A], z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B = fold1(as, m.zero)((b, a) => m.op(b, f(a)))
}

object mapReduce{
  def apply[F[_], A, B](as: F[A])(f: A => B)(implicit fd: Foldable[F], m: Monoid[B]): B = fd.foldMap(as)(f)
}