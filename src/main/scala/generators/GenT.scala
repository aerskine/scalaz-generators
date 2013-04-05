package generators

import scalaz._
import scalaz.effect._

object GenT extends GenTFunctions with GenTInstances {
  type GenT[M[+_], E, +A] = ReaderT[M, Consumer[M, E], A]
  type Producer[M[+_], E] = GenT[M, E, Unit]
  type Consumer[M[+_], E] = E => M[Unit]
  type Transducer[M[+_], N[+_], E1, E2] = Producer[M, E1] => Producer[N, E2]

  def apply[M[+_], E, A](f: Consumer[M, E] => M[A]): GenT[M, E, A] = genT(f)
}

import GenT.GenT

trait GenTFunctions {
  import GenT._
  def genT[M[+_], E, A](f: Consumer[M, E] => M[A]): GenT[M, E, A] = Kleisli(f)
  def liftG[M[+_] : Monad, E, A](ma: M[A]): GenT[M, E, A] = GenT { (consumer: Consumer[M, E]) => ma }
  def `yield`[M[+_] : Monad, E](e: E): Producer[M, E] = {
    import Kleisli._
    for (consumer <- ask[M, Consumer[M, E]]; res <- liftG(consumer(e))) yield res
  }
  def runGenT[F[+_] : Monad, E](producer: Producer[F, E], consumer: Consumer[F, E]): F[Unit] = producer run consumer
}

trait GenTInstances0 {
  implicit def GenTMonad[F[+_], E](implicit F0: Monad[F]): Monad[({type λ[+α] = GenT[F, E, α]})#λ] = new GenTMonad[F, E] {
    implicit def F = F0
  }
}

trait GenTInstances extends GenTInstances0 {

}

private[generators] trait GenTMonad[F[+_], E] extends Monad[({type λ[+α] = GenT[F, E, α]})#λ] {
  implicit def F: Monad[F]

  def point[A](a: => A) = sys.error("todo")
  def bind[A, B](fa: GenT[F, E, A])(f: A => GenT[F, E, B]): GenT[F, E, B] = fa flatMap f
}

/** A port of: http://okmij.org/ftp/continuations/PPYield/GenT.hs
  */
object GenTHs extends SafeApp {
  sealed trait Doc {
    def |++|(d: Doc) = Concat(this, d)
  }
  case class Text(s: String) extends Doc
  case object Line extends Doc
  case class Concat(d1: Doc, d2: Doc) extends Doc
  case class Group(d: Doc) extends Doc

  val doc1 = Group { Text("A") |++|
    (Line |++|
      Group(Text("B") |++|
        (Line |++| Text("C"))))
  }

  import IO._
  import syntax.monad._
  /** Traverse the document tree and print out the node as we traversed them
    * Show in the XML-like way
    */
  def traverse1(doc: Doc): IO[Unit] = doc match {
    case d@Text(_) => putStrLn(d.toString)
    case d@Line => putStrLn(d.toString())
    case Concat(d1, d2) => traverse1(d1) >> traverse1(d2)
    case Group(d) => putStrLn("Begin Group") >> traverse1(d) >> putStrLn("End Group")
  }
  /*
  Begin Group
  Text(A)
  Line
  Begin Group
  Text(B)
  Line
  Text(C)
  End Group
  End Group
   */

  /** Do the same with generators
    * We just globally replaced putStrLn with yield
    * (and changed the signature)
    */
  def traverse2[M[+_] : Monad](doc: Doc): GenT[M, String, Unit] = {
    def `yield`(s: String) = GenT.`yield`[M, String](s)
    doc match {
      case d@Text(_) => `yield`(d.toString)
      case d@Line => `yield`(d.toString)
      case Concat(d1, d2) => traverse2[M](d1) >> traverse2[M](d2)
      case Group(d) => `yield`("Begin Group") >> traverse2(d) >> `yield`("End Group")
    }
  }
  def tt1 = traverse1(doc1)

  /** One should not be surprised in how we run this
    * (which instantiates yield as putStrLn)
    */
  def tt2 = GenT.runGenT(traverse2[IO](doc1), putStrLn)
  override def runc = {
    tt1 >> putStrLn("---") >> tt2
  }
}
