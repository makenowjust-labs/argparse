package codes.quine.labo.argparse

import scala.collection.mutable

final case class Help(
    usage: Usage,
    help: String,
    options: Seq[(String, String)],
    subcommands: Seq[(String, String)],
    positionals: Seq[(String, String)]
) {
  override def toString: String = {
    val sb = new mutable.StringBuilder
    def group(name: String, infos: Seq[(String, String)]): Unit =
      if (infos.nonEmpty) {
        sb.append(s"$name:\n")
        infos.foreach {
          case (name, help) if name.size <= 20 => sb.append(f"    $name%-20s $help\n")
          case (name, help)                    => sb.append(s"    $name\n    ${" " * 20} $help\n")
        }
        sb.append("\n")
      }
    sb.append(s"${usage.commandNames.mkString(" ")} - $help\n\n")
    sb.append(s"Usage:\n    $usage\n\n")
    group("Options", options)
    group("Subcommands", subcommands)
    group("Positional Arguments", positionals)
    sb.result()
  }
}
