package codes.quine.labo.argparse.prelude

object ApplicativeLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Applicative[F], eqF: Equiv[F[A]]): Boolean =
    eqF.equiv(F.ap(F.pure((a: A) => a), fa), fa)

  def composition[F[_], A, B, C](fa: F[A], ff: F[A => B], fg: F[B => C])(implicit
      F: Applicative[F],
      eqF: Equiv[F[C]]
  ): Boolean = {
    val lhs = F.ap(F.ap(F.ap(F.pure[(B => C) => (A => B) => (A => C)](g => f => g.compose(f)), fg), ff), fa)
    val rhs = F.ap(fg, F.ap(ff, fa))
    eqF.equiv(lhs, rhs)
  }

  def homomorphism[F[_], A, B](a: A, f: A => B)(implicit F: Applicative[F], eqF: Equiv[F[B]]): Boolean =
    eqF.equiv(F.ap(F.pure(f), F.pure(a)), F.pure(f(a)))

  def interchange[F[_], A, B](a: A, ff: F[A => B])(implicit F: Applicative[F], eqF: Equiv[F[B]]): Boolean =
    eqF.equiv(F.ap(ff, F.pure(a)), F.map(ff)(_(a)))
}
