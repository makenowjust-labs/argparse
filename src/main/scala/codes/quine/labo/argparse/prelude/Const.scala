package codes.quine.labo.argparse.prelude

import scala.annotation.nowarn

private[argparse] final case class Const[R, A](value: R) {
  @nowarn("msg=never used")
  def map[B](f: A => B): Const[R, B] = Const(value)
}

object Const {
  @nowarn("msg=never used")
  def pure[R, A](value: A)(implicit R: Monoid[R]): Const[R, A] = Const(R.empty)

  def ap[R, A, B](ff: Const[R, A => B], fa: Const[R, A])(implicit R: Monoid[R]): Const[R, B] =
    Const(R.combine(fa.value, ff.value))

  implicit def applicative[R: Monoid]: Applicative[({ type F[A] = Const[R, A] })#F] =
    new Applicative[({ type F[A] = Const[R, A] })#F] {
      def map[A, B](fa: Const[R, A])(f: A => B): Const[R, B] = fa.map(f)
      def pure[A](value: A): Const[R, A] = Const.pure(value)
      def ap[A, B](ff: Const[R, A => B], fa: Const[R, A]): Const[R, B] = Const.ap(ff, fa)
    }
}
