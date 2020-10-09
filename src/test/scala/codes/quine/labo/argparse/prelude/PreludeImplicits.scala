package codes.quine.labo.argparse.prelude

import codes.quine.labo.hariko.Gen

object PreludeImplicits {
  implicit def idGen[A: Gen]: Gen[Id[A]] = Gen[A].map(Id(_))

  implicit def idEquiv[A: Equiv]: Equiv[Id[A]] = Equiv.by(_.value)

  implicit def constGen[R: Gen, A]: Gen[Const[R, A]] = Gen[R].map(Const(_))

  implicit def constEquiv[R: Equiv, A]: Equiv[Const[R, A]] = Equiv.by(_.value)

  implicit def freeApplicatviveGen[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[FreeApplicative[F, A]] =
    Gen.either(F, A).map {
      case Left(fa) => FreeApplicative.lift(fa)
      case Right(a) => FreeApplicative.pure(a)
    }

  implicit def freeApplicativeEquiv[F[_]: Applicative, A](implicit eqF: Equiv[F[A]]): Equiv[FreeApplicative[F, A]] =
    Equiv.by(_.foldMap(FunctionK.identity[F]))

  implicit def seqGen[A: Gen]: Gen[Seq[A]] = Gen.list[A].map(_.toSeq)

  implicit def eitherEquiv[A: Equiv, B: Equiv]: Equiv[Either[A, B]] = Equiv.fromFunction {
    case (Left(x), Left(y))   => Equiv[A].equiv(x, y)
    case (Right(x), Right(y)) => Equiv[B].equiv(x, y)
    case _                    => false
  }
}
