import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.{Await, Future}

/**
  * Created by marco on 12/04/17.
  */

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ApplicativeSpec extends WordSpec with Matchers {


  "Aplicative" should {

    import cats.Applicative
    import cats.instances.future._
    import cats.instances.option._
    import scala.concurrent.Future

    "compose " in {

      val x: Future[Option[Int]] = Future.successful(Some(5))
      val y: Future[Option[Char]] = Future.successful(Some('a'))

      val result = Await.result(Applicative[Future].compose[Option].map2(x, y)(_ + _), 1 second)

      result shouldBe Some(102)

    }

    "compose nested" in {

      import java.sql.Connection
      import cats.Applicative
      import cats.instances.option._

      val username: Option[String] = Some("username")
      val password: Option[String] = Some("password")
      val url: Option[String] = Some("some.login.url.here")

      def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

      val result = Applicative[Option].map3(username, password, url)(attemptConnect)

      result shouldBe Some(None)
    }

    "treaverse option " in {

      import cats.Applicative
      import cats.instances.option._

      def traverseOption[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {

        list.foldLeft(Option(List.empty[B]))(
          (acc, el) => {
            val fa: Option[B] = f(el)
            acc.map(_ ++ List(f(el).get))
          })
      }

      val result = traverseOption(List("a", "a", "a", "a"))(str => Some(str))

      result shouldBe Some(List("a", "a", "a", "a"))


    }

  }

}




