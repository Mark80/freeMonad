import java.io.FileWriter

import TaskMonad._
import cats.Monad
import fs2.Strategy

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

//trait Functor[F[_]]
//
//type IntKeyMap[A] = Map[Int, A]
//
//type F4 = Functor[({type T[A] = Map[Int, A]})#T]
//
//def foo[A[_, _], B](functor: Functor[({type AB[C] = A[B, C]})#AB])

trait ~>[F[_], G[_]] {

  def apply[A](fa: F[A]): G[A]

}


sealed trait Free[F[_], A] {

  import Free._

  def map[B](f: A => B): Free[F, B] =
    Map(this, f)

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    Bind(this, f)


  def foldMap[G[_] : Monad](nt: F ~> G): G[A] = this match {

    case Suspend(fa) => nt(fa)
    case Pure(a) => Monad[G].pure(a)
    case Bind(target, f) =>
      val ge = target.foldMap(nt) // G[E]
      Monad[G].flatMap(ge) { e =>
        val fa = f(e) // Free[F[A],A]
        fa.foldMap(nt) // G[A]
      }
    case Map(target, f) =>
      val ge = target.foldMap(nt) // G[E]
      Monad[G].map(ge) { e =>
        f(e)
      }

  }


}


object Free {

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  final case class Pure[F[_], A](a: A) extends Free[F, A]

  final case class Suspend[F[_], A](a: F[A]) extends Free[F, A]

  final case class Bind[F[_], E, A](target: Free[F, E], f: E => Free[F, A]) extends Free[F, A]

  final case class Map[F[_], E, A](target: Free[F, E], f: E => A) extends Free[F, A]

}


sealed trait DiskIO[A]

final case class Read(a: String) extends DiskIO[Array[Byte]]

final case class Write(a: String, content: Array[Byte]) extends DiskIO[Unit]

import fs2.Task


object TaskMonad {

  implicit val  strategy = Strategy.fromExecutionContext(implicitly[ExecutionContext])

  implicit val  evidenceByte = new Monad[Task] {

    override def pure[A](x: A) =
    Task(x)

    override def flatMap[A, B](fa: Task[A])(f: (A) => Task[B]) =
    fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: (A) => Task[Either[A, B]]) = ???


  }

}


object Ex {


  import Free._

  def main(args: Array[String]): Unit = {


    val free = for {
      c1 <- liftF(Read("a.txt"))
      c2 <- liftF(Read("b.txt"))
      _ <-  liftF(Write("c.txt", c1 ++ c2))
    } yield ()

    free.foldMap(interpreter).unsafeRunSync()



  }


  private def interpreter() = {

    new (DiskIO ~> Task) {

      override def apply[A](fa: DiskIO[A]) = fa match {

        case Read(fileName) => Task.delay {
          val content = scala.io.Source.fromFile(fileName).mkString
          content.getBytes
        }

        case Write(file, content) => Task.delay {
          val writer = new FileWriter(file)
          writer.write(content.toString)
          writer.flush()
        }

      }

    }
  }



}




