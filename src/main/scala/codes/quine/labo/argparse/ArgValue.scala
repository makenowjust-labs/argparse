package codes.quine.labo.argparse

trait ArgValue[A] {
  def read(value: String): Either[Seq[String], A]

  def defaultMetavar: String
}

object ArgValue {
  implicit val string: ArgValue[String] = new ArgValue[String] {
    def read(value: String): Either[Seq[String], String] = Right(value)
    def defaultMetavar: String = "<string>"
  }
}
