package codes.quine.labo.argparse.prelude

private[argparse] trait FunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object FunctionK {
  def identity[F[_]]: FunctionK[F, F] = new FunctionK[F, F] {
    def apply[A](fa: F[A]): F[A] = fa
  }
}
