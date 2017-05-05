//import org.scalatest.{Matchers, WordSpec}
//import cats.free.Free
//import cats.free.Free.liftF
//import cats.{Id, ~>}
//import scala.collection.mutable
//
//
//class FreeMonadSpec extends WordSpec with Matchers {
//
//  type KVStore[A] = Free[KVStoreA, A]
//
//
//  "Free monad " should {
//
//    "execute composed computation" in {
//
//      val program: KVStore[Option[Int]] =
//        for {
//          _ <- put("wild-cats", 2)
//          _ <- update[Int]("wild-cats", _ + 12)
//          _ <- put("tame-cats", 5)
//          n <- get[Int]("wild-cats")
//          _ <- delete("tame-cats")
//        } yield n
//
//
//      program.foldMap(impureCompiler) shouldBe Some(14)
//
//    }
//
//  }
//
//
//  def put[T](key: String, value: T): KVStore[Unit] =
//    liftF[KVStoreA, Unit](Put[T](key, value))
//
//  def get[T](key: String): KVStore[Option[T]] =
//    liftF[KVStoreA, Option[T]](Get[T](key))
//
//  def delete[T](key: String): KVStore[Unit] =
//    liftF[KVStoreA, Unit](Delete(key))
//
//  def update[T](key: String, f: T => T): KVStore[Unit] =
//    for {
//      opt <- get[T](key)
//      _ <- opt.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
//    } yield ()
//
//
//  def impureCompiler: KVStoreA ~> Id  =
//    new (KVStoreA ~> Id) {
//
//      val kvs = mutable.Map.empty[String, Any]
//
//      def apply[A](fa: KVStoreA[A]): Id[A] =
//        fa match {
//          case Put(key, value) =>
//            println(s"put($key, $value)")
//            kvs(key) = value
//            ()
//          case Get(key) =>
//            println(s"get($key)")
//            kvs.get(key).map(_.asInstanceOf[A])
//          case Delete(key) =>
//            println(s"delete($key)")
//            kvs.remove(key)
//            ()
//        }
//    }
//}
//
//
//sealed trait KVStoreA[A]
//
//case class Put[T](key: String, value: T) extends KVStoreA[Unit]
//
//case class Get[T](key: String) extends KVStoreA[Option[T]]
//
//case class Delete(key: String) extends KVStoreA[Unit]
//
//
//
//
