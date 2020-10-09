package codes.quine.labo.argparse.prelude

import scala.math.Equiv.Implicits.seqEquiv

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import PreludeImplicits._

object ConstSpec extends SimpleTestSuite with HarikoChecker {
  test("Const#value") {
    assertEquals(Const(42).value, 42)
  }

  test("Const#map") {
    assertEquals(Const(42).map((_: Int) + 1), Const(42))
  }

  test("Const.pure") {
    assertEquals(Const.pure[Seq[Int], Int](42), Const(Seq.empty))
  }

  test("Const.ap") {
    assertEquals(Const.ap(Const[Seq[Int], Int => Int](Seq(2)), Const[Seq[Int], Int](Seq(1))), Const(Seq(1, 2)))
  }

  test("Const: Applicative") {
    type F[A] = Const[Seq[Int], A]
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
