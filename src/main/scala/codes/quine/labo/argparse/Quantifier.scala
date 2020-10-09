package codes.quine.labo.argparse

import prelude.Id

sealed abstract class Quantifier[A] {
  import Quantifier.Validation

  private[argparse] def accept(input: Input): Match[Quantifier, A]

  private[argparse] def result: Either[Seq[Failure], A]

  def usage: String

  def args: Seq[Arg[_]]

  final def validate[B](f: A => Either[Seq[String], B]): Quantifier[B] = this match {
    case Validation(arg, g) => Validation(arg, g.andThen(_.flatMap(f)))
    case arg                => Validation(arg, f)
  }

  final def map[B](f: A => B): Quantifier[B] = validate(a => Right(f(a)))
}

object Quantifier {
  import Match.{Done, Read, ReadAll, NoMatch, Ambiguous}
  import Failure.{InvalidValue, MissingArgument}

  private[argparse] final case class Required[A](group: Group[A], acc: Option[A] = None) extends Quantifier[A] {
    def accept(input: Input): Match[Quantifier, A] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => Required(group, Some(value)) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => Required(group, Some(value)) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => Required(group, Some(value)) })
      case None                 => NoMatch(this)
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], A] = acc match {
      case Some(value) => Right(value)
      case None        => Left(Seq(MissingArgument(group.simpleNames)))
    }

    def usage: String = group.usages match {
      case Seq(usage) => usage
      case usages     => usages.mkString("(", " | ", ")")
    }

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class Optional[A](group: Group[A], acc: Option[A] = None) extends Quantifier[Option[A]] {
    def accept(input: Input): Match[Quantifier, Option[A]] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => Optional(group, Some(value)) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => Optional(group, Some(value)) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => Optional(group, Some(value)) })
      case None                 => NoMatch(this: Quantifier[Option[A]])
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], Option[A]] = Right(acc)

    def usage: String = group.usages.mkString("[", " | ", "]")

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class RequiredMany[A](group: Group[A], acc: Seq[A] = Seq.empty)
      extends Quantifier[Seq[A]] {
    def accept(input: Input): Match[Quantifier, Seq[A]] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => RequiredMany(group, acc :+ value) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => RequiredMany(group, acc :+ value) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => RequiredMany(group, acc :+ value) })
      case None                 => NoMatch(this: Quantifier[Seq[A]])
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], Seq[A]] =
      if (acc.nonEmpty) Right(acc)
      else Left(Seq(MissingArgument(group.simpleNames)))

    def usage: String = group.usages match {
      case Seq(usage) => s"$usage..."
      case usages     => usages.mkString("(", " | ", ")...")
    }

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class OptionalMany[A](group: Group[A], acc: Seq[A] = Seq.empty)
      extends Quantifier[Seq[A]] {
    def accept(input: Input): Match[Quantifier, Seq[A]] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => OptionalMany(group, acc :+ value) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => OptionalMany(group, acc :+ value) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => OptionalMany(group, acc :+ value) })
      case None                 => NoMatch(this: Quantifier[Seq[A]])
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], Seq[A]] = Right(acc)

    def usage: String = group.usages.mkString("[", " | ", "]...")

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class RequiredOnce[A](group: Group[A]) extends Quantifier[A] {
    def accept(input: Input): Match[Quantifier, A] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => Accepted(value) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => Accepted(value) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => Accepted(value) })
      case None                 => NoMatch(this)
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], A] = Left(Seq(MissingArgument(group.simpleNames)))

    def usage: String = group.usages match {
      case Seq(usage) => usage
      case usages     => usages.mkString("(", " | ", ")")
    }

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class OptionalOnce[A](group: Group[A]) extends Quantifier[Option[A]] {
    def accept(input: Input): Match[Quantifier, Option[A]] = group.accept(input) match {
      case Some(Done(result))   => Done(result.map { case Id(value) => Accepted(Some(value)) })
      case Some(Read(read))     => Read(read(_).map { case Id(value) => Accepted(Some(value)) })
      case Some(ReadAll(reads)) => ReadAll(reads(_).map { case Id(value) => Accepted(Some(value)) })
      case None                 => NoMatch(this: Quantifier[Option[A]])
      case Some(_)              => sys.error("internal error")
    }

    def result: Either[Seq[Failure], Option[A]] = Right(None)

    def usage: String = group.usages.mkString("[", " | ", "]")

    def args: Seq[Arg[_]] = group.args
  }

  private[argparse] final case class Accepted[A](value: A) extends Quantifier[A] {
    def accept(input: Input): Match[Quantifier, A] = NoMatch(this)

    def result: Either[Seq[Failure], A] = Right(value)

    def usage: String = sys.error("internal error")

    def args: Seq[Arg[_]] = sys.error("internal error")
  }

  private[argparse] final case class Validation[A, B](quantifier: Quantifier[A], f: A => Either[Seq[String], B])
      extends Quantifier[B] {
    def accept(input: Input): Match[Quantifier, B] = quantifier.accept(input) match {
      case Done(result)   => Done(result.map(Validation(_, f)))
      case Read(read)     => Read(read(_).map(Validation(_, f)))
      case ReadAll(reads) => ReadAll(reads(_).map(Validation(_, f)))
      case NoMatch(set)   => NoMatch(Validation(set, f))
      case Ambiguous()    => Ambiguous()
    }

    def result: Either[Seq[Failure], B] =
      quantifier.result.flatMap(f(_).left.map(_.map(InvalidValue(None, _))))

    def usage: String = quantifier.usage

    def args: Seq[Arg[_]] = quantifier.args
  }
}
