package codes.quine.labo.argparse

sealed abstract class FlagName

object FlagName {
  final case class ShortName(name: Char) extends FlagName {
    override def toString: String = s"-$name"
  }

  final case class LongName(name: String) extends FlagName {
    override def toString: String = s"--$name"
  }
}
