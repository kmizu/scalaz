package scalaz

////
/**
 *
 */
////
trait MonadRec[F[_]] extends Monad[F] { self =>
  ////

  // tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b
  def tailRecM[A, B](f: A => F[A \/ B]): A => F[B]

  ////
  val monadRecSyntax = new scalaz.syntax.MonadRecSyntax[F] { def F = MonadRec.this }
}

object MonadRec {
  @inline def apply[F[_]](implicit F: MonadRec[F]): MonadRec[F] = F

  ////


  ////
}
