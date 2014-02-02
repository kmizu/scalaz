package scalaz
package effect

////
/**
 *
 */
////
trait LiftIO[F[_]]  { self =>
  ////

  def liftIO[A](ioa: IO[A]): F[A]

  // derived functions

  ////
  val liftIOSyntax = new scalaz.syntax.effect.LiftIOSyntax[F] { def F = LiftIO.this }
}

private abstract class AbstractLiftIO[F[_]] extends LiftIO[F]

object LiftIO {
  @inline def apply[F[_]](implicit F: LiftIO[F]): LiftIO[F] = F

  ////
  
  ////
}
