package codes.quine.labo.argparse.prelude

private[argparse] sealed abstract class FreeApplicative[F[_], A] {
  import FreeApplicative.{Pure, Ap}

  final def map[B](f: A => B): FreeApplicative[F, B] = this match {
    case Pure(value) => Pure(f(value))
    case Ap(fa, apf) => Ap(fa, apf.map(_.andThen(f)))
  }

  final def mapK[G[_]](t: FunctionK[F, G]): FreeApplicative[G, A] = this match {
    case Pure(value) => Pure(value)
    case Ap(fa, apf) => Ap(t(fa), apf.mapK(t))
  }

  final def foldMap[G[_]](t: FunctionK[F, G])(implicit G: Applicative[G]): G[A] = this match {
    case Pure(value) => G.pure(value)
    case Ap(fa, apf) => G.ap(apf.foldMap(t), t(fa))
  }
}

private[argparse] object FreeApplicative {
  final case class Pure[F[_], A](value: A) extends FreeApplicative[F, A]

  final case class Ap[F[_], A, B](fa: F[A], apf: FreeApplicative[F, A => B]) extends FreeApplicative[F, B]

  private[this] def flip[A, B, C](f: A => B => C): B => A => C =
    (b: B) => (a: A) => f(a)(b)

  def pure[F[_], A](value: A): FreeApplicative[F, A] = Pure(value)

  def lift[F[_], A](fa: F[A]): FreeApplicative[F, A] = Ap(fa, Pure(identity(_: A)))

  def ap[F[_], A, B](apf: FreeApplicative[F, A => B], apa: FreeApplicative[F, A]): FreeApplicative[F, B] =
    (apf, apa) match {
      case (Pure(f), apa)     => apa.map(f)
      case (apf, Pure(a))     => apf.map(_(a))
      case (Ap(fa, apf), apa) => Ap(fa, ap(apf.map(flip(_)), apa))
    }

  implicit def applicative[F[_]]: Applicative[({ type G[A] = FreeApplicative[F, A] })#G] =
    new Applicative[({ type G[A] = FreeApplicative[F, A] })#G] {
      override def map[A, B](fa: FreeApplicative[F, A])(f: A => B): FreeApplicative[F, B] = fa.map(f)
      def pure[A](value: A): FreeApplicative[F, A] = FreeApplicative.pure(value)
      def ap[A, B](ff: FreeApplicative[F, A => B], fa: FreeApplicative[F, A]): FreeApplicative[F, B] =
        FreeApplicative.ap(ff, fa)
    }
}
