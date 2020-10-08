package codes.quine.labo.argparse.prelude

private[argparse] trait Applicative[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f), fa)

  def pure[A](value: A): F[A]

  def ap[A, B](ff: F[A => B], fa: F[A]): F[B]
}

private[argparse] object Applicative {
  implicit def either[R](implicit R: Monoid[R]): Applicative[({ type G[A] = Either[R, A] })#G] =
    new Applicative[({ type G[A] = Either[R, A] })#G] {
      override def map[A, B](fa: Either[R, A])(f: A => B): Either[R, B] = fa.map(f)
      def pure[A](value: A): Either[R, A] = Right(value)
      def ap[A, B](ff: Either[R, A => B], fa: Either[R, A]): Either[R, B] = (ff, fa) match {
        case (Right(f), Right(a)) => Right(f(a))
        case (Left(l), Left(r))   => Left(R.combine(l, r))
        case (Left(l), _)         => Left(l)
        case (_, Left(r))         => Left(r)
      }
    }
}
