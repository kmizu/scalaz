package scalaz
package syntax

final class MonadErrorIdOps[S](private val self: S) extends AnyVal {
  def raiseError[F[_], A](implicit F: MonadError[F, S]): F[A] =
    F.raiseError[A](self)
}
