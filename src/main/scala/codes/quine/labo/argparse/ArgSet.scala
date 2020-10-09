package codes.quine.labo.argparse

import scala.annotation.tailrec

import prelude.Const
import prelude.FreeApplicative
import prelude.FunctionK

final case class ArgSet[A](set: FreeApplicative[Quantifier, A]) {
  import Arg.{OptionFlag, SwitchFlag, Positional, Subcommand}
  import ArgSet.{FA, MFA}
  import Input.{InputFlag, InputPositional, InputSubcommand}
  import Match.{Done, Read, ReadAll, Ambiguous}
  import Failure.{AmbiguousArgument, UnknownArgument, MissingValue}
  import FlagName.{ShortName, LongName}

  def parse(args: Seq[String]): Either[Seq[Failure], A] = {
    def accept(input: Input): FunctionK[Quantifier, MFA] = new FunctionK[Quantifier, MFA] {
      def apply[A](fa: Quantifier[A]): MFA[A] = fa
        .accept(input)
        .mapK(new FunctionK[Quantifier, FA] {
          def apply[A](fa: Quantifier[A]): FA[A] = FreeApplicative.lift(fa)
        })
    }

    def handleFlagWithValue(set: FA[A], flag: FlagName, value: String): Either[Seq[Failure], FA[A]] =
      set.foldMap(accept(InputFlag(flag))) match {
        case Read(read)  => read(value)
        case Ambiguous() => Left(Seq(AmbiguousArgument(flag.toString)))
        case _           => Left(Seq(UnknownArgument(flag.toString)))
      }

    def handleFlag(set: FA[A], flag: FlagName, args: Seq[String]): (Either[Seq[Failure], FA[A]], Seq[String]) =
      set.foldMap(accept(InputFlag(flag))) match {
        case Read(read) =>
          args match {
            case Seq()         => (Left(Seq(MissingValue(flag.toString))), Seq())
            case value +: args => (read(value), args)
          }
        case Done(result) => (result, args)
        case Ambiguous()  => (Left(Seq(AmbiguousArgument(flag.toString))), args)
        case _            => (Left(Seq(UnknownArgument(flag.toString))), args)
      }

    def handleSubcommand(set: FA[A], name: String, args: Seq[String]): Option[Either[Seq[Failure], FA[A]]] =
      set.foldMap(accept(InputSubcommand(name))) match {
        case ReadAll(reads) => Some(reads(args))
        case Ambiguous()    => Some(Left(Seq(AmbiguousArgument(name))))
        case _              => None
      }

    def handlePositional(set: FA[A], value: String): Either[Seq[Failure], FA[A]] =
      set.foldMap(accept(InputPositional(value))) match {
        case Done(result) => result
        case Ambiguous()  => Left(Seq(AmbiguousArgument(value)))
        case _            => Left(Seq(UnknownArgument(value)))
      }

    @tailrec def consumePositionals(set: FA[A], args: Seq[String]): Either[Seq[Failure], FA[A]] = args match {
      case Seq() => Right(set)
      case value +: args =>
        handlePositional(set, value) match {
          case Left(fs)   => Left(fs)
          case Right(set) => consumePositionals(set, args)
        }
    }

    def handleShortFlag(
        set: FA[A],
        flag: FlagName,
        arg: String,
        args: Seq[String]
    ): (Either[Seq[Failure], FA[A]], Seq[String]) =
      set.foldMap(accept(InputFlag(flag))) match {
        case Done(result) => (result, s"-$arg" +: args)
        case Read(read)   => (read(arg), args)
        case Ambiguous()  => (Left(Seq(AmbiguousArgument(flag.toString))), s"-$arg" +: args)
        case _            => (Left(Seq(UnknownArgument(flag.toString))), s"-$arg" +: args)
      }

    @tailrec def loop(set: FA[A], args: Seq[String]): Either[Seq[Failure], A] =
      if (args.isEmpty) result(set)
      else {
        val (next, nextArgs) = args match {
          case "--" +: args                        => (consumePositionals(set, args), Seq())
          case "-" +: args                         => (handlePositional(set, "-"), args)
          case arg +: args if arg.startsWith("-=") => (handlePositional(set, arg), args)
          case s"--$name=$value" +: args           => (handleFlagWithValue(set, LongName(name), value), args)
          case s"--$name" +: args                  => handleFlag(set, LongName(name), args)
          case s"-$name" +: args =>
            (name.head, name.tail) match {
              case (c, "")                         => handleFlag(set, ShortName(c), args)
              case (c, arg) if arg.startsWith("=") => (handleFlagWithValue(set, ShortName(c), arg.tail), args)
              case (c, arg)                        => handleShortFlag(set, ShortName(c), arg, args)
            }
          case arg +: args =>
            handleSubcommand(set, arg, args).map((_, Seq())).getOrElse((handlePositional(set, arg), args))
        }

        next match {
          case Left(fs)   => Left(fs)
          case Right(set) => loop(set, nextArgs)
        }
      }

    def result(set: FA[A]): Either[Seq[Failure], A] =
      set.foldMap(new FunctionK[Quantifier, ({ type G[A] = Either[Seq[Failure], A] })#G] {
        def apply[A](fa: Quantifier[A]): Either[Seq[Failure], A] = fa.result
      })

    loop(set, args)
  }

  def quantifiers: Seq[Quantifier[_]] =
    set
      .foldMap(new FunctionK[Quantifier, ({ type G[A] = Const[Seq[Quantifier[_]], A] })#G] {
        def apply[A](fa: Quantifier[A]): Const[Seq[Quantifier[_]], A] = Const(Seq(fa))
      })
      .value

  def args: Seq[Arg[_]] = quantifiers.flatMap(_.args)

  def usage(commandNames: Seq[String]): Usage = Usage(commandNames, quantifiers)

  def help(commandNames: Seq[String], help: String): Help = {
    val options = Seq.newBuilder[(String, String)]
    val subcommands = Seq.newBuilder[(String, String)]
    val positionals = Seq.newBuilder[(String, String)]
    args.foreach {
      case flag: OptionFlag[_]   => options.addOne(flag.info)
      case flag: SwitchFlag[_]   => options.addOne(flag.info)
      case sub: Subcommand[_, _] => subcommands.addOne(sub.info)
      case pos: Positional[_]    => positionals.addOne(pos.info)
    }
    Help(usage(commandNames), help, options.result(), subcommands.result(), positionals.result())
  }

  def subcommands: Map[String, ArgSet[_]] = {
    val subcommands = Map.newBuilder[String, ArgSet[_]]
    args.foreach {
      case sub: Subcommand[_, _] =>
        sub.names.foreach(name => subcommands.addOne(name -> sub.set))
      case _ => ()
    }
    subcommands.result()
  }
}

object ArgSet {
  private type FA[A] = FreeApplicative[Quantifier, A]
  private type MFA[A] = Match[FA, A]

  def pure[A](value: A): ArgSet[A] = ArgSet(FreeApplicative.pure(value))

  def ap[A, B](ff: ArgSet[A => B], fa: ArgSet[A]): ArgSet[B] =
    ArgSet(FreeApplicative.ap(ff.set, fa.set))

  def map2[A1, A2, B](fa1: ArgSet[A1], fa2: ArgSet[A2])(f: (A1, A2) => B): ArgSet[B] =
    ap(ap(pure((a1: A1) => (a2: A2) => f(a1, a2)), fa1), fa2)

  def map3[A1, A2, A3, B](fa1: ArgSet[A1], fa2: ArgSet[A2], fa3: ArgSet[A3])(f: (A1, A2, A3) => B): ArgSet[B] =
    ap(map2(fa1, fa2)((a1, a2) => f(a1, a2, _)), fa3)

  def map4[A1, A2, A3, A4, B](fa1: ArgSet[A1], fa2: ArgSet[A2], fa3: ArgSet[A3], fa4: ArgSet[A4])(
      f: (A1, A2, A3, A4) => B
  ): ArgSet[B] =
    ap(map3(fa1, fa2, fa3)((a1, a2, a3) => f(a1, a2, a3, _)), fa4)

  def map5[A1, A2, A3, A4, A5, B](fa1: ArgSet[A1], fa2: ArgSet[A2], fa3: ArgSet[A3], fa4: ArgSet[A4], fa5: ArgSet[A5])(
      f: (A1, A2, A3, A4, A5) => B
  ): ArgSet[B] =
    ap(map4(fa1, fa2, fa3, fa4)((a1, a2, a3, a4) => f(a1, a2, a3, a4, _)), fa5)
}
