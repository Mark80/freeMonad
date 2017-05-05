import cats.data.EitherT
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._


import scala.concurrent.duration._
/**
  * Created by marco on 11/04/17.
  */
class FunctorSpec extends WordSpec with Matchers {

  type FutureT[A] = EitherT[Future, Throwable, A]

  "Functor " should {


    "combine int" in {

      val result: EitherT[Future, Throwable, String] = for {
        f <- foo()
        b <- buzz()
      } yield f + b

      Await.result(result.value, 1 second) match {
        case Right(value) => value shouldBe "foobuzz"
        case _ => fail()
      }

    }


  }


  def foo(): FutureT[String] = EitherT.pure[Future, Throwable, String]("foo")

  def buzz(): FutureT[String] = EitherT.pure[Future, Throwable, String]("buzz")


}
