package codes.quine.labo.argparse

import scala.math.Equiv.Implicits._

import codes.quine.labo.hariko.Cogen
import codes.quine.labo.hariko.Gen

import Failure._
import Match._
import prelude.PreludeImplicits._

object ArgparseImplicits {
  implicit def seqCogen[A: Cogen]: Cogen[Seq[A]] =
    Cogen[List[A]].imap(_.toSeq: Seq[A])(_.toList)

  implicit def functionEquiv[A: Gen, B: Equiv]: Equiv[A => B] =
    Equiv.fromFunction { (f, g) =>
      Gen[A].samples().take(30).forall(x => Equiv[B].equiv(f(x), g(x)))
    }

  implicit def failureGen: Gen[Failure] =
    Gen.frequency(
      1 -> Gen[String].map(AmbiguousArgument(_)),
      1 -> Gen[String].map(UnknownArgument(_)),
      1 -> Gen[String].map(MissingValue(_)),
      1 -> Gen.map2(Gen[Option[String]], Gen[String])(InvalidValue(_, _)),
      1 -> Gen[Seq[String]].map(MissingArgument(_))
    )

  implicit def failureEquiv: Equiv[Failure] = Equiv.universal

  implicit def matchGen[F[_], A](implicit F: Gen[F[A]]): Gen[Match[F, A]] =
    Gen.frequency(
      1 -> Gen[Either[Seq[Failure], F[A]]].map(Done(_)),
      1 -> Gen[String => Either[Seq[Failure], F[A]]].map(Read(_)),
      1 -> Gen[Seq[String] => Either[Seq[Failure], F[A]]].map(ReadAll(_)),
      1 -> Gen[F[A]].map(NoMatch(_)),
      1 -> Gen.pure(Ambiguous())
    )

  implicit def matchEquiv[F[_], A](implicit F: Equiv[F[A]]): Equiv[Match[F, A]] =
    Equiv.fromFunction {
      case (Done(x), Done(y))         => Equiv[Either[Seq[Failure], F[A]]].equiv(x, y)
      case (Read(x), Read(y))         => Equiv[String => Either[Seq[Failure], F[A]]].equiv(x, y)
      case (ReadAll(x), ReadAll(y))   => Equiv[Seq[String] => Either[Seq[Failure], F[A]]].equiv(x, y)
      case (NoMatch(x), NoMatch(y))   => Equiv[F[A]].equiv(x, y)
      case (Ambiguous(), Ambiguous()) => true
      case _                          => false
    }
}
