package codes.quine.labo.argparse.prelude

private[argparse] final case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
}

private[argparse] object Id {
  def pure[A](value: A): Id[A] = Id(value)

  def ap[A, B](ff: Id[A => B], fa: Id[A]): Id[B] = Id(ff.value(fa.value))

  implicit val applicative: Applicative[Id] = new Applicative[Id] {
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = fa.map(f)
    def pure[A](value: A): Id[A] = Id.pure(value)
    def ap[A, B](ff: Id[A => B], fa: Id[A]): Id[B] = Id.ap(ff, fa)
  }
}
