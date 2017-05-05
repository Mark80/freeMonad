import org.scalatest.enablers.Emptiness
import org.scalatest.{Matchers, WordSpec}

import org.mockito.Mockito._


/**
  * Created by marco on 12/04/17.
  */
class CacheSpec extends WordSpec with Matchers {

  implicit def toEmpty(cache: Cache[_, _]): Emptiness[Cache[_, _]] =
    (thing: Cache[_, _]) => thing.isEmpty


  "Cache " should {

    "be empty at startp" in {

      val cache = new Cache[String, String](identity)
      cache shouldBe empty

    }


    "call a service if key is required" in {

      val service: String => Int = s => s.length

      val cache = new Cache[String, Int](service)

      cache.get("eccomi")

      cache.size shouldBe 1
      cache.isEmpty shouldBe false
    }

    "not call a service if key is found" in {

      val service: String => Int = mock(classOf[Function[String, Int]])

      val cache = new Cache[String, Int](service)

      cache.get("eccomi")
      cache.get("eccomi")
      cache.get("eccomi")

      verify(service, times(1)).apply("eccomi")
    }



  }

}


class Cache[K, V](service: K => V) {

  private var store = Map.empty[K, V]
  private var empty = true

  def size() = store.size

  def isEmpty = empty

  def get(key: K) = store.getOrElse(key, callServiceAndStoreValue(key))

  private def callServiceAndStoreValue(key: K) = {
    val value = service(key)
    store = store + (key -> value)
    empty = false
    value
  }


}