package org.banking.collection

class NonEmptyList[A] extends List[A]{
  override def productElement(n: Int): Any = ???

  override def productArity: Int = ???
}
