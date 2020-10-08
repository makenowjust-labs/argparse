package codes.quine.labo.argparse

import prelude.Id

sealed abstract class Arg[A] {
  private[argparse] def accept(input: Input): Option[Match[Id, A]]

  def validate[B](f: A => Either[Seq[String], B]): Arg[B]

  def simpleName: String

  final def map[B](f: A => B): Arg[B] = validate(a => Right(f(a)))
}

object Arg {
  import Input.{InputFlag, InputPositional, InputSubcommand}
  import Match.{Done, Read, ReadAll}
  import Failure.InvalidValue

  private[argparse] final case class OptionFlag[A](
      names: Seq[FlagName],
      metavar: String,
      help: String,
      read: String => Either[Seq[String], A]
  ) extends Arg[A] {
    def validate[B](f: A => Either[Seq[String], B]): Arg[B] =
      copy(read = read(_).flatMap(f))

    def accept(input: Input): Option[Match[Id, A]] = input match {
      case InputFlag(name) if names.contains(name) =>
        Some(Read(read(_).left.map(_.map(InvalidValue(Some(name.toString), _))).map(Id(_))))
      case _ => None
    }

    def simpleName: String = names.head.toString
  }

  private[argparse] final case class SwitchFlag[A](
      names: Seq[FlagName],
      help: String,
      read: () => Either[Seq[String], A]
  ) extends Arg[A] {
    def validate[B](f: A => Either[Seq[String], B]): Arg[B] =
      copy(read = () => read().flatMap(f))

    def accept(input: Input): Option[Match[Id, A]] = input match {
      case InputFlag(name) if names.contains(name) =>
        Some(Done(read().left.map(_.map(InvalidValue(Some(name.toString), _))).map(Id(_))))
      case _ => None
    }

    def simpleName: String = names.head.toString
  }

  private[argparse] final case class Positional[A](
      metavar: String,
      help: String,
      read: String => Either[Seq[String], A]
  ) extends Arg[A] {
    def validate[B](f: A => Either[Seq[String], B]): Arg[B] =
      copy(read = read(_).flatMap(f))

    def accept(input: Input): Option[Match[Id, A]] = input match {
      case InputPositional(value) =>
        Some(Done(read(value).left.map(_.map(InvalidValue(None, _))).map(Id(_))))
      case _ => None
    }

    def simpleName: String = metavar
  }

  private[argparse] final case class Subcommand[A, B](
      names: Seq[String],
      help: String,
      set: ArgSet[A],
      reads: A => Either[Seq[Failure], B]
  ) extends Arg[B] {
    def validate[C](f: B => Either[Seq[String], C]): Arg[C] =
      copy(reads = reads(_: A).flatMap(f(_).left.map(_.map(InvalidValue(None, _)))))

    def accept(input: Input): Option[Match[Id, B]] = input match {
      case InputSubcommand(name) if names.contains(name) =>
        Some(ReadAll(set.parse(_).flatMap(reads(_).map(Id(_)))))
      case _ => None
    }

    def simpleName: String = names.head
  }
}
