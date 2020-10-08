package codes.quine.labo.argparse

sealed abstract class Failure {
  def errorMessage: String
}

object Failure {
  final case class AmbiguousArgument(argument: String) extends Failure {
    def errorMessage: String = s"ambiguous argument: $argument"
  }

  final case class UnknownArgument(argument: String) extends Failure {
    def errorMessage: String = s"unknown argument: $argument"
  }

  final case class MissingValue(name: String) extends Failure {
    def errorMessage: String = s"missing value: $name"
  }

  final case class InvalidValue(name: Option[String], message: String) extends Failure {
    def errorMessage: String =
      s"invalid value${name.map(": " + _).getOrElse("")}: $message"
  }

  final case class MissingArgument(names: Seq[String]) extends Failure {
    def errorMessage: String = names match {
      case Seq(name)     => s"missing required argument: $name"
      case names :+ name => s"missing required argument: ${names.mkString(", ")} or $name"
      case Seq()         => s"missing required argument"
    }
  }
}
