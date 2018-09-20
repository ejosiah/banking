package org.banking.fp

trait Monoid[T] {
  def zero: T
  def op(t0: T, t1: T): T
}
