package codes.quine.labo.argparse
package prelude

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import PreludeImplicits._

object IdSuite extends SimpleTestSuite with HarikoChecker {
  test("Id.pure") {
    assertEquals(Id.pure(42), Id(42))
  }

  test("Id.ap") {
    assertEquals(Id.ap(Id((_: Int) + 1), Id(41)), Id(42))
  }

  test("Id#value") {
    assertEquals(Id(42).value, 42)
  }

  test("Id#map") {
    assertEquals(Id(1).map(_ + 41), Id(42))
  }

  test("Id: Applicative laws") {
    type F[A] = Id[A]
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
