package org.banking.investment

trait Trading[Account, Market, Order, ClientOrder, Execution, Trade] {

  def clientOrders: ClientOrder => List[Order]

  def execute(m: Market, a: Account):  Order => List[Execution]

  def allocate(as: List[Account]): Execution => List[Trade]
}
