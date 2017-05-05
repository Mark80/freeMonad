import org.scalatest.{Matchers, WordSpec}

/**
  * Created by marco on 11/04/17.
  */
class MonoidSpec  extends WordSpec with Matchers {

  import cats.Monoid

  "Monoid " should {

    "combine int" in {

      implicit val intMonoid = new Monoid[Int]{
        override def empty = 0
        override def combine(x: Int, y: Int) = x + y
      }

      Monoid[Int].combine(3,7) shouldBe 10

    }

    "combine int with syntax" in {

      import cats.syntax.monoid._
      import cats.instances.int._

      3 |+| 7 shouldBe 10

    }

    "combine option" in {

      import cats.Functor
      import cats.instances.list._
      import cats.instances.option._

      val listOption = List(Some(1), None, Some(2))

      val result = Functor[List].compose[Option].map(listOption)(_ + 1)

      result shouldBe List(Some(2), None, Some(3))

    }

  }

}
