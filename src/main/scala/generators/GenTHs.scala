package generators

import scalaz.effect.IO._
import scalaz.effect._
import scalaz._

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

  val doc1 = Group(Text("A") |++|
    (Line |++|
      Group(Text("B") |++|
        (Line |++|
          Text("C")))))

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

  import GenT._
  /** Do the same with generators
    * We just globally replaced putStrLn with yield
    * (and changed the signature)
    */
  def traverse2[M[+_] : Monad](doc: Doc): Producer[M, String] = {
    def `yield`(s: String) = GenT.`yield`[M, String](s)
    doc match {
      case d@Text(_) =>
        `yield`(d.toString)
      case d@Line =>
        `yield`(d.toString)
      case Concat(d1, d2) =>
        traverse2[M](d1) >> traverse2[M](d2)
      case Group(d) =>
        `yield`("Begin Group") >> traverse2(d) >> `yield`("End Group")
    }
  }
  def tt1 = traverse1(doc1)

  /** One should not be surprised in how we run this
    * (which instantiates yield as putStrLn)
    */
  def tt2 = GenT.runGenT(traverse2[IO](doc1))(putStrLn)

  /** A small test of the online behavior (of foldG) */
  def tf1: IO[Unit] = {
    type GenTIO[+a] = GenT[IO, Int, a]
    type ProducerIO = GenTIO[Unit]
    type StateTGenTIO[+a] = StateT[GenTIO, Unit, a]
    def f(u: Unit, e: Int): ProducerIO = {
      MonadIO[GenTIO].liftIO(putStrLn("f: " + e)) >> `yield`(e)
    }
    val gen = `yield`[StateTGenTIO, Int](1) >> `yield`(2)
    val gen1 = foldG[GenTIO, Unit, Int](f)(())(gen)
    runGenT(gen1) { i=> putStrLn("Generated: " + i) }
  }
  /*
  {-
    f: 1
    Generated: 1
    f: 2
    Generated: 2
    -}
    */

  /** A more interesting example, transforming the stream of strings
    * to the stream of running lengths of the strings
    */
  trait TranW[M[+_]] {
    implicit def M: MonadIO[M]
    type GenTInt[+a] = GenT[M, Int, a]
    type StateTGenTInt[+a] = StateT[GenTInt, Int, a]
    type ProducerSGIO = Producer[StateTGenTInt, String]
    def f(acc: Int, str: String): GenTInt[Int] =
      for {
        _ <- MonadIO[GenTInt].liftIO(putStr(str) >> putStr(": "))
        acc1 = acc + str.length
        _ <- `yield`[M, Int](acc1)
      } yield acc1
    def tranW(producer: ProducerSGIO): GenTInt[Unit] =
      foldG_[GenTInt, Int, String](f)(0)(producer)
  }

  def tW = {
    def print(i: Int): IO[Unit] = putStrLn(i.toString)
    val out = new TranW[IO] {
      implicit def M = MonadIO[IO]
      def run() = runGenT(tranW(traverse2(doc1)))(print)
    }
    out.run()
  }
  //  tW = runGenT (tranW (traverse2 doc1)) print
  //    {-
  //      Begin Group: 11
  //      Text "A": 19
  //      Line: 23
  //      Begin Group: 34
  //      Text "B": 42
  //      Line: 46
  //      Text "C": 54
  //      End Group: 63
  //      End Group: 72
  //      -}

  override def runc = (
    putStrLn("tt1: manual traverse1 on doc1")
      >> tt1 >> putStrLn("---")
      >> putStrLn("tt2: generator traverse2 on doc1")
      >> tt2 >> putStrLn("---")
      >> putStrLn("tf1: foldG on yield(1) >> yield(2)")
      >> tf1 >> putStrLn("---")
      >> putStrLn("tW: transform doc1 into running sequence of lengths")
      >> tW >> putStrLn("---")
    )
}
