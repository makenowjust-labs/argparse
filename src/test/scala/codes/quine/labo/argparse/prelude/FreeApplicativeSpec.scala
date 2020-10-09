package codes.quine.labo.argparse.prelude

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import FreeApplicative.Pure
import PreludeImplicits._

object FreeApplicativeSpec extends SimpleTestSuite with HarikoChecker {
  test("FreeApplicative#foldMap") {
    val ap = FreeApplicative.lift(Id(42))
    assertEquals(
      ap.foldMap(new FunctionK[Id, ({ type F[A] = Const[Seq[Int], A] })#F] {
        def apply[A](fa: Id[A]): Const[Seq[Int], A] = Const(Seq(1))
      }),
      Const(Seq(1))
    )
  }

  test("FreeApplicative.pure") {
    assertEquals(FreeApplicative.pure[Id, Int](42), Pure[Id, Int](42))
  }

  test("FreeApplicative.ap") {
    val apf = FreeApplicative.pure[Id, Int => Int]((_: Int) + 1)
    val apa = FreeApplicative.pure[Id, Int](41)
    assertEquals(FreeApplicative.ap(apf, apa), Pure(42))
  }

  test("FreeApplicative: Applicative") {
    type F[A] = FreeApplicative[Id, A]
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
