package cats.mtl.laws.discipline

import cats.Eq
import cats.mtl.laws.ComonadStoreLaws
import org.scalacheck.Prop.{forAll => ∀}
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import cats.mtl.ComonadStore

trait ComonadStoreTests[F[_], S] extends Laws {
  implicit val storeInstance: ComonadStore[F, S]

  def laws: ComonadStoreLaws[F, S] = ComonadStoreLaws[F, S]

  def monadState[A: Arbitrary, B: Arbitrary](implicit
                               ArbFA: Arbitrary[F[A]],
                               ArbFAB: Arbitrary[F[A] => B],
                               ArbFS: Arbitrary[F[S]],
                               ArbS: Arbitrary[S],
                               EqA: Eq[A],
                               EqS: Eq[S],
                               EqFA: Eq[F[A]],
                               EqFS: Eq[F[S]],
                              ): RuleSet = {
    new DefaultRuleSet(
      name = "comonadStore",
      parent = None,
      "pos then seek does nothing" -> ∀(laws.posThenSeekDoesNothing[A] _),
      "seek then pos does nothing" -> ∀(laws.seekThenPosDoesNothing _),
      "seek then seek seeks once" -> ∀(laws.seekThenSeekSeeksOnce[A] _),
      "extract is peek" -> ∀(laws.extractIsPeek[A] _),
      "pos is coflatMap then pos" -> ∀(laws.posIsCoflatMapThenPos[A, B] _)
    )
  }

}

object ComonadStoreTests {
  def apply[F[_], S](implicit instance0: ComonadStore[F, S]): ComonadStoreTests[F, S] = {
    new ComonadStoreTests[F, S] with Laws {
      override implicit val storeInstance: ComonadStore[F, S] = instance0
    }
  }
}
