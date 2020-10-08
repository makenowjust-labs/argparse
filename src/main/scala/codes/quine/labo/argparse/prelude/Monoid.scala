package codes.quine.labo.argparse.prelude

private[argparse] trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

private[argparse] object Monoid {
  implicit def seq[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
    def empty: Seq[A] = Seq.empty
    def combine(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
  }
}
