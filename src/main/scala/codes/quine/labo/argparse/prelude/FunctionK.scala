package codes.quine.labo.argparse.prelude

private[argparse] trait FunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
