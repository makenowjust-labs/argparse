package codes.quine.labo.argparse

import codes.quine.labo.hariko.Property
import codes.quine.labo.hariko.minitest.HarikoChecker
import minitest.SimpleTestSuite

import ArgparseImplicits._
import Match._
import prelude.PreludeImplicits._
import prelude.Id
import prelude.ApplicativeLaws
import prelude.FunctionK

object MatchSuite extends SimpleTestSuite with HarikoChecker {
  test("Match#mapK") {
    val identity = new FunctionK[Id, Id] {
      def apply[A](fa: Id[A]): Id[A] = fa
    }
    assertEquals(Done(Right(Id(42))).mapK(identity), Done(Right(Id(42))))
    assertEquals(Ambiguous().mapK(identity), Ambiguous())
  }

  test("Match: Applicative laws") {
    type F[A] = Match[Id, A]
    check(Property.forAll[F[Int]](ApplicativeLaws.identity(_)).withParam(_.copy(minSuccessful = 50)))
    check(
      Property
        .forAll[(F[Int], F[Int => Int], F[Int => Int])] { case (fa, ff, fg) =>
          ApplicativeLaws.composition(fa, ff, fg)
        }
        .withParam(_.copy(minSuccessful = 50))
    )
    check(
      Property
        .forAll[(Int, Int => Int)] { case (a, f) =>
          ApplicativeLaws.homomorphism[F, Int, Int](a, f)
        }
        .withParam(_.copy(minSuccessful = 50))
    )
    check(
      Property
        .forAll[(Int, F[Int => Int])] { case (a, ff) =>
          ApplicativeLaws.interchange(a, ff)
        }
        .withParam(_.copy(minSuccessful = 50))
    )
  }
}
