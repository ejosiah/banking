package org.banking

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits._

object Application extends App {
  import java.util.Date

  import cats.Applicative

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: List[E] = Nil) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  object Validation{
    type Valid[A] = Validation[String, A]

    implicit def app: Applicative[Valid] = new Applicative[Valid] {
      def pure[A](x: A): Valid[A] = Success(x)

      def ap[A, B](ff: Valid[A => B])(fa: Valid[A]): Valid[B] = (fa, ff) match {
        case (Failure(h0, t0), Failure(h1, t1)) => Failure(h0, h1 :: t0 ::: t1)
        case (Success(a), Success(f)) => Success(f(a))
        case (Success(_), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(_)) => f
      }
    }
  }

  import Validation._

  case class WebForm(name: String, dob: Date, phoneNumber: String)

  def validate(name: String): Valid[String] = {
    if(name != "") Success(name)
    else Failure("Name cannot be empty")
  }

  def validateDob(dob: String): Valid[Date] = {
    try{
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(dob))
    }catch{
      case ex: Exception => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  }

  def validatePhone(phoneNumber: String): Valid[String] = {
    if(phoneNumber.matches("[0-9]{11}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")
  }



  def validateWebForm(name: String, dob: String, phone: String)(implicit app: Applicative[Valid]): Valid[WebForm] = {
    app.map3(
      validate(name),
      validateDob(dob),
      validatePhone(phone)
    )(WebForm)
  }

  implicit def appFuture: Applicative[Future] = new Applicative[Future] {
    override def pure[A](x: A): Future[A] = Future.successful(x)

    override def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] = for{
      a <- fa
      f <- ff
    } yield f(a)
  }

  println(validateWebForm("", "1983-09", "078542992301"))

  val f = appFuture.map3(
    Future{ println("Hello World") },
    Future.failed[Unit](new Exception("Sorry, something went wrong")),
    Future{ println(5 * 8)}
  )((_, _, _) => ())

  val a = Await.result(f, 10 seconds)
  println(a)

  val appOp = Applicative[Option]
}
