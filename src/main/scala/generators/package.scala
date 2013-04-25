import scalaz._

package object generators {
  type GenT[M[+_], E, +A] = ReaderT[M, Consumer[M, E], A]
  type Producer[M[+_], E] = GenT[M, E, Unit]
  type Consumer[M[+_], E] = E => M[Unit]
  type Transducer[M[+_], N[+_], E1, E2] = Producer[M, E1] => Producer[N, E2]
}
