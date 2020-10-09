package codes.quine.labo.argparse

import prelude.Applicative
import prelude.FunctionK

private[argparse] sealed abstract class Match[F[_], A] {
  import Match.{Done, Read, ReadAll, NoMatch, Ambiguous}

  final def map[B](f: A => B)(implicit F: Applicative[F]): Match[F, B] = this match {
    case Done(result)   => Done(result.map(F.map(_)(f)))
    case Read(read)     => Read(read(_).map(F.map(_)(f)))
    case ReadAll(reads) => ReadAll(reads(_).map(F.map(_)(f)))
    case NoMatch(set)   => NoMatch(F.map(set)(f))
    case Ambiguous()    => Ambiguous()
  }

  final def mapK[G[_]](t: FunctionK[F, G]): Match[G, A] = this match {
    case Done(result)   => Done(result.map(t(_)))
    case Read(read)     => Read(read(_).map(t(_)))
    case ReadAll(reads) => ReadAll(reads(_).map(t(_)))
    case NoMatch(set)   => NoMatch(t(set))
    case Ambiguous()    => Ambiguous()
  }
}

private[argparse] object Match {
  final case class Done[F[_], A](result: Either[Seq[Failure], F[A]]) extends Match[F, A]
  final case class Read[F[_], A](read: String => Either[Seq[Failure], F[A]]) extends Match[F, A]
  final case class ReadAll[F[_], A](reads: Seq[String] => Either[Seq[Failure], F[A]]) extends Match[F, A]
  final case class NoMatch[F[_], A](set: F[A]) extends Match[F, A]
  final case class Ambiguous[F[_], A]() extends Match[F, A]

  def pure[F[_], A](value: A)(implicit F: Applicative[F]): Match[F, A] = NoMatch(F.pure(value))

  def ap[F[_], A, B](ff: Match[F, A => B], fa: Match[F, A])(implicit F: Applicative[F]): Match[F, B] =
    (ff, fa) match {
      case (Done(result), NoMatch(r))   => Done(result.map(F.ap(_, r)))
      case (NoMatch(l), Done(result))   => Done(result.map(F.ap(l, _)))
      case (Read(read), NoMatch(r))     => Read(read(_).map(F.ap(_, r)))
      case (NoMatch(l), Read(read))     => Read(read(_).map(F.ap(l, _)))
      case (ReadAll(reads), NoMatch(r)) => ReadAll(reads(_).map(F.ap(_, r)))
      case (NoMatch(l), ReadAll(reads)) => ReadAll(reads(_).map(F.ap(l, _)))
      case (NoMatch(l), NoMatch(r))     => NoMatch(F.ap(l, r))
      case (_, _)                       => Ambiguous()
    }

  implicit def applicative[F[_]: Applicative]: Applicative[({ type G[A] = Match[F, A] })#G] =
    new Applicative[({ type G[A] = Match[F, A] })#G] {
      def map[A, B](fa: Match[F, A])(f: A => B): Match[F, B] = fa.map(f)
      def pure[A](value: A): Match[F, A] = Match.pure(value)
      def ap[A, B](ff: Match[F, A => B], fa: Match[F, A]): Match[F, B] = Match.ap(ff, fa)
    }
}
