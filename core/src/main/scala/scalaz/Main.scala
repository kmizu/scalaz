package scalaz

final case class Trace[A](a: A) {
  def map[B](f: A => B): Trace[B] = Trace(f(a))
}

object Trace {
  implicit val instance: Functor[Trace] =
    new Functor[Trace] {
      def map[A, B](fa: Trace[A])(f: A => B) =
        fa map f
    }

  def trace[A](a: A): Main.TraceF[A] = Free.pure(a)

}

// https://github.com/purescript/purescript-tailrec/blob/v0.2.0/test/Main.purs#L44-L52
object Main {
  type TraceF[A] = Free[Trace, A]

  implicit val traceFMonadRec: MonadRec[TraceF] =
    new MonadRec[TraceF]{
      override def point[A](a: => A) =
        Free.point(a)
      override def bind[A, B](fa: TraceF[A])(f: A => TraceF[B]) =
        fa flatMap f
      def tailRecM[A, B](f: A => TraceF[A \/ B]): A => TraceF[B] = {
        @annotation.tailrec
        def loop(fa: Free[Trace, A \/ B]): TraceF[B] = fa.resume match {
          case \/-(\/-(b)) =>
            point(b)
          case \/-(-\/(aa)) =>
            loop(f(aa))
          case -\/(Trace(fa)) =>
            loop(fa)
        }

        (a: A) => loop(f(a))
      }
    }

  import std.AllInstances._

  def loopWriter(i: BigInt): WriterT[TraceF, BigInt, String] = {
    val M = WriterT.writerTHoist[BigInt]
    import syntax.either._

    type Result = WriterT[TraceF, BigInt, BigInt \/ String]

    val go: BigInt => Result = {
      case n if n == BigInt(0) =>
        for{
          a <- M.liftM(Trace.trace("Done!"))
        } yield \/-(a)
      case n =>
        val W = WriterT.writerTMonadListen[TraceF, BigInt]
        for{
          _ <- W.tell(n)
        } yield -\/(n - 1)
    }

    WriterT.writerTMonadRec[TraceF, BigInt].tailRecM(go).apply(i)
  }

  def main(args: Array[String]): Unit = {
    println(loopWriter(1000))
  }
}
