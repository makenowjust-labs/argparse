package codes.quine.labo

import scala.language.implicitConversions

package object argparse {
  import prelude.FreeApplicative

  import Arg.{OptionFlag, SwitchFlag, Positional, Subcommand}
  import FlagName.{ShortName, LongName}
  import Quantifier.{Required, Optional, RequiredMany, OptionalMany, RequiredOnce, OptionalOnce}

  implicit def argToGroup[A](arg: Arg[A]): Group[A] = Group(None, Seq(arg))
  implicit def argToQuantifier[A](arg: Arg[A]): Quantifier[A] = groupToQuantifier(argToGroup(arg))
  implicit def argToArgSet[A](arg: Arg[A]): ArgSet[A] = quantifierToArgSet(groupToQuantifier(argToGroup(arg)))

  implicit def groupToQuantifier[A](group: Group[A]): Quantifier[A] = required(group, once = true)
  implicit def groupToArgSet[A](group: Group[A]): ArgSet[A] = quantifierToArgSet(groupToQuantifier(group))

  implicit def quantifierToArgSet[A](fa: Quantifier[A]): ArgSet[A] = ArgSet(FreeApplicative.lift(fa))

  def option[A](name: String, shortName: String = "", metavar: String = "", help: String = "")(implicit
      A: ArgValue[A]
  ): Arg[A] =
    OptionFlag(
      Seq(LongName(name)) ++ shortName.map(ShortName(_)),
      if (metavar.isEmpty) A.defaultMetavar else metavar,
      help,
      A.read(_)
    )

  def switch(name: String, shortName: String = "", help: String = ""): Arg[Unit] = SwitchFlag(
    Seq(LongName(name)) ++ shortName.map(ShortName(_)),
    help,
    () => Right(())
  )

  def positional[A](metavar: String = "", help: String = "")(implicit A: ArgValue[A]): Arg[A] =
    Positional(
      if (metavar.isEmpty) A.defaultMetavar else metavar,
      help,
      A.read(_)
    )

  def subcommand[A](name: String, set: ArgSet[A], alias: Seq[String] = Seq.empty, help: String = ""): Arg[A] =
    Subcommand(
      Seq(name) ++ alias,
      help,
      set,
      (a: A) => Right(a)
    )

  def group[A](name: String, group: Group[A]): Group[A] = Group(Some(name), group.args)

  def required[A](group: Group[A], once: Boolean = false): Quantifier[A] =
    if (once) RequiredOnce(group) else Required(group)

  def optional[A](group: Group[A]): Quantifier[Option[A]] = optional(group, false)

  def optional[A](group: Group[A], default: A): Quantifier[A] = optional(group, default, false)

  def optional[A](group: Group[A], once: Boolean): Quantifier[Option[A]] =
    if (once) OptionalOnce(group) else Optional(group)

  def optional[A](group: Group[A], default: A, once: Boolean): Quantifier[A] =
    optional(group, once).map(_.getOrElse(default))

  def many[A](group: Group[A]): Quantifier[Seq[A]] = OptionalMany(group)

  def some[A](group: Group[A]): Quantifier[Seq[A]] = RequiredMany(group)
}
