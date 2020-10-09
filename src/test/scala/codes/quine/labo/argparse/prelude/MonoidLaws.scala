package codes.quine.labo.argparse.prelude

object MonoidLaws {
  def leftIdentity[A](x: A)(implicit A: Monoid[A], eqA: Equiv[A]): Boolean =
    eqA.equiv(A.combine(A.empty, x), x)

  def rightIdentity[A](x: A)(implicit A: Monoid[A], eqA: Equiv[A]): Boolean =
    eqA.equiv(A.combine(x, A.empty), x)

  def associativity[A](x: A, y: A, z: A)(implicit A: Monoid[A], eqA: Equiv[A]): Boolean =
    eqA.equiv(A.combine(A.combine(x, y), z), A.combine(x, A.combine(y, z)))
}
