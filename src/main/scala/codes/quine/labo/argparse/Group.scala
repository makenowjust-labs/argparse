package codes.quine.labo.argparse

import prelude.Id

final case class Group[A](name: Option[String], args: Seq[Arg[A]]) {
  import Match.Ambiguous

  private[argparse] def accept(input: Input): Option[Match[Id, A]] = args
    .map(_.accept(input))
    .collect { case Some(m) => m } match {
    case Seq()  => None
    case Seq(m) => Some(m)
    case _      => Some(Ambiguous())
  }

  def validate[B](f: A => Either[Seq[String], B]): Group[B] = Group(name, args.map(_.validate(f)))

  def <|>(that: Group[A]): Group[A] = Group(name.orElse(that.name), args ++ that.args)

  def map[B](f: A => B): Group[B] = validate(a => Right(f(a)))

  def simpleNames: Seq[String] = name.map(Seq(_)).getOrElse(args.map(_.simpleName))

  def usages: Seq[String] = name.map(Seq(_)).getOrElse(args.map(_.usage))
}
