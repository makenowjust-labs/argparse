package codes.quine.labo.argparse

final case class Usage(commandNames: Seq[String], quantifiers: Seq[Quantifier[_]]) {
  override def toString: String =
    (commandNames ++ quantifiers.map(_.usage)).mkString(" ")
}
