package org.banking.commerce

import java.util.Date

import org.banking.commerce.domain._

import scala.util.{Failure, Success, Try}

trait AccountService[Account, Amount, Balance] {

  def open(no: String, name: String, openDate: Option[Date]): Try[Account]

  def close(account: Account, closeDate: Option[Date]): Try[Account]

  def debit(account: Account, amount: Amount): Try[Account]

  def credit(account: Account, amount: Amount): Try[Account]

  def balance(account: Account): Try[Balance]

  def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] = for{
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield (a, b, amount)
}

object AccountService extends AccountService[Account, Amount, Balance]{

  def open(no: String, name: String, openDate: Option[Date] = Some(today)): Try[Account] = {
    if(no.isEmpty || name.isEmpty) Failure( new Exception(s"Account no or name cannot be blank"))
    else if (openDate.get before today) Failure(new Exception("Cannot open account before today"))
    else Success(Account(no, name, openDate.get))
  }

  def close(account: Account, closeDate: Option[Date]): Try[Account] = ???

  def debit(account: Account, amount: Amount): Try[Account] = ???

  def credit(account: Account, amount: Amount): Try[Account] = ???

  def balance(account: Account): Try[Balance] = ???
}
