package codes.quine.labo.argparse

private[argparse] sealed abstract class Input

private[argparse] object Input {
  final case class InputFlag(name: FlagName) extends Input

  final case class InputPositional(value: String) extends Input

  final case class InputSubcommand(name: String) extends Input
}
