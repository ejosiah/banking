package org.banking.commerce

import java.util.{Calendar, Date}

object domain {
  def today = Calendar.getInstance().getTime
  type Amount = BigDecimal
  case class Balance(amount: Amount = 0)
  case class Account(no: String, name: String, dateOfOpen: Date, dateOfClose: Option[Date] = None, balance: Balance = Balance(0))
}
