import org.scalatest.{Matchers, WordSpec}

/**
  * Created by marco on 11/04/17.
  */
class SemigroupSpec extends WordSpec with Matchers {

  import cats.Semigroup
  import cats.syntax.semigroup._


  "Semigroup" should {

    "combine 2 integer" in {

      implicit val intSemigroup = new Semigroup[Int] {
        override def combine(x: Int, y: Int) = x + y
      }

      Semigroup[Int].combine(3, 5) shouldBe 8

    }

    "combine 2 integer with syntax" in {

      implicit val intSemigroup = new Semigroup[Int] {
        override def combine(x: Int, y: Int) = x + y
      }

      3 |+| 5 shouldBe 8

    }

    "combine 2 integer with syntax built in implicit" in {

      import cats.instances.all._

      3 |+| 5 shouldBe 8

    }

    "combine 2 map" in {

      import cats.instances.map._

      val map1 = Map("hello" -> 0, "world" -> 1)
      val map2 = Map("hello" -> 2, "cats" -> 3)

      implicit val intSemigroup = new Semigroup[Int] {
        override def combine(x: Int, y: Int) = x + y
      }

      val result = Semigroup[Map[String, Int]].combine(map1, map2)

      result shouldBe Map("hello" -> 2, "world" -> 1, "cats" -> 3)

      map1 |+| map2 shouldBe Map("hello" -> 2, "world" -> 1, "cats" -> 3)

    }

    "merge option" in {

      import cats.instances.all._

      def mergeOpt[A: Semigroup](a: A, opt: Option[A]) : A = opt.map(a |+| _).getOrElse(a)

      mergeOpt(2, Some(3))  shouldBe 5


    }

  }

}
