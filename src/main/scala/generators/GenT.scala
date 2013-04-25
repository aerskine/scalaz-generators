package generators

import scalaz._
import scalaz.effect._

object GenT extends GenTFunctions with GenTInstances {
  def apply[M[+_], E, A](f: Consumer[M, E] => M[A]): GenT[M, E, A] = genT(f)
}

trait GenTFunctions {
  def genT[M[+_], E, A](f: Consumer[M, E] => M[A]): GenT[M, E, A] = Kleisli(f)
  def liftG[M[+_] : Monad, E, A](ma: M[A]): GenT[M, E, A] = genT { (consumer: Consumer[M, E]) => ma }
  def `yield`[M[+_] : Monad, E](e: E): Producer[M, E] = {
    import Kleisli._
    for (consumer <- ask[M, Consumer[M, E]]; res <- liftG(consumer(e))) yield res
  }
  def runGenT[M[+_] : Monad, E](producer: Producer[M, E])(consumer: Consumer[M, E]): M[Unit] = producer run consumer
  def mapG[M[+_], E1, E2](f: E1 => E2)(implicit M: Monad[M]): Transducer[GenTM[M, E2]#λ, M, E1, E2] = {
    def fConsumer(x: E1): GenT[M, E2, Unit] = `yield`(f(x))
    producer => runGenT[GenTM[M, E2]#λ, E1](producer)(fConsumer)
  }
  def foldG[M[+ _], S, E](
    f: (S, E) => M[S])(
    s0: S)(
    producer: Producer[({type l[+a] = StateT[M, S, a]})#l, E])(
    implicit M: Monad[M]
  ): M[S] = {
    val MS = MonadState[({type f[s, +a] = StateT[M, s, a]})#f, S]
    def lift(ma: M[S]): StateT[M, S, S] =
      StateT(s => M.map(ma)(a => (s, a)))
    def consumer(x: E): StateT[M, S, Unit] = {
      def fs(s: S): StateT[M, S, S] = lift(f(s, x))
      import syntax.monad._
      MS.get >>= fs >>= MS.put
    }
    runGenT[({type l[+a] = StateT[M, S, a]})#l, E](producer)(consumer).exec(s0)
  }
  def foldG_[M[+ _], S, E](
    f: (S, E) => M[S])(
    s0: S)(
    producer: Producer[({type l[+a] = StateT[M, S, a]})#l, E])(
    implicit M: Monad[M]
  ): M[Unit] = {
    val MS = MonadState[({type f[s, +a] = StateT[M, s, a]})#f, S]
    def lift(ma: M[S]): StateT[M, S, S] =
      StateT(s => M.map(ma)(a => (s, a)))
    def consumer(x: E): StateT[M, S, Unit] = {
      def fs(s: S): StateT[M, S, S] = lift(f(s, x))
      import syntax.monad._
      MS.get >>= fs >>= MS.put
    }
    runGenT[({type l[+a] = StateT[M, S, a]})#l, E](producer)(consumer).eval(s0)
  }
}

trait GenTInstances0 {
  implicit def GenTMonad[M[+_], E](implicit F0: Monad[M]): Monad[({type λ[+α] = GenT[M, E, α]})#λ] = new GenTMonad[M, E] {
    implicit def M = F0
  }
}

trait GenTInstances extends GenTInstances0 {
  implicit def GenTMonadTrans[E]: MonadTrans[({type λ[β[+_], α] = GenT[β, E, α]})#λ] =
    new GenTMonadTrans[E] { }
  implicit def GenTMonadIO[M[+_], E](implicit M0: MonadIO[M]): MonadIO[({type λ[α] = GenT[M, E, α]})#λ] =
    new GenTMonadIO[M, E] {
      implicit def M = M0
    }
}

private[generators] class GenTM[M[+_], E] {
  type λ[+α] = GenT[M, E, α]
}

private[generators] trait GenTMonad[M[+_], E] extends Monad[GenTM[M, E]#λ] {
  implicit def M: Monad[M]

  def point[A](a: => A) = GenT.genT[M, E, A](consumer => M.point(a))
  def bind[A, B](fa: GenT[M, E, A])(f: A => GenT[M, E, B]): GenT[M, E, B] = fa flatMap f
  override def map[A, B](fa: GenT[M, E, A])(f: A => B): GenT[M, E, B] = fa map f
}

private[generators] trait GenTMonadTrans[E] extends MonadTrans[({type λ[β[+_], α] = GenT[β, E, α]})#λ] {
  trait GenTF[F[+_]] {
    type λ[α] = GenT[F, E, α]
  }

  def liftM[M[+_] : Monad, A](ma: M[A]): GenT[M, E, A] =
    GenT(consumer => ma)

  implicit def apply[M[+_] : Monad]: Monad[GenTF[M]#λ] = GenT.GenTMonad[M, E]
}

private[generators] trait GenTMonadIO[M[+_], E] extends MonadIO[({type λ[α] = GenT[M, E, α]})#λ] with GenTMonad[M, E] {
  implicit def M: MonadIO[M]
  import GenT._
  def liftIO[A](ioa: IO[A]) = MonadTrans[({type λ[α[+_], β] = GenT[α, E, β]})#λ].liftM(M.liftIO(ioa))
}
