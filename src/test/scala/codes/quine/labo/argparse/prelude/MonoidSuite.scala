package codes.quine.labo.argparse.prelude

import scala.math.Equiv.Implicits.seqEquiv

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import PreludeImplicits._

object MonoidSuite extends SimpleTestSuite with HarikoChecker {
  test("Monoid.seq: Monoid laws") {
    type A = Seq[Int]
    check(Property.forAll[A](MonoidLaws.leftIdentity(_)))
    check(Property.forAll[A](MonoidLaws.rightIdentity(_)))
    check(Property.forAll[(A, A, A)] { case (x, y, z) =>
      MonoidLaws.associativity(x, y, z)
    })
  }
}
