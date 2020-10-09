package codes.quine.labo.argparse.prelude

import scala.math.Equiv.Implicits.seqEquiv

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import PreludeImplicits._

object ApplicativeSpec extends SimpleTestSuite with HarikoChecker {
  test("Applicative.either: Applicative laws") {
    type F[A] = Either[Seq[Int], A]
    check(Property.forAll[F[Int]](ApplicativeLaws.identity(_)))
    check(Property.forAll[(F[Int], F[Int => Int], F[Int => Int])] { case (fa, ff, fg) =>
      ApplicativeLaws.composition(fa, ff, fg)
    })
    check(Property.forAll[(Int, Int => Int)] { case (a, f) =>
      ApplicativeLaws.homomorphism[F, Int, Int](a, f)
    })
    check(Property.forAll[(Int, F[Int => Int])] { case (a, ff) =>
      ApplicativeLaws.interchange(a, ff)
    })
  }
}
