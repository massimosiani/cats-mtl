package cats.mtl.tests

import cats.arrow.FunctionK
import cats.data.{Kleisli, StateT}
import cats.instances.all._
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary._
import cats.mtl.instances.eithert._
import cats.mtl.instances.optiont._
import cats.mtl.instances.readert._
import cats.mtl.instances.state._
import cats.mtl.instances.writert._
import cats.mtl.laws.discipline.{MonadLayerControlTests, MonadStateTests}
import cats.mtl.lifting.MonadLayerControl
import cats.~>
import org.scalacheck.{Arbitrary, Gen}

class StoreTestsBase extends BaseSuite {
  implicit val arbFunctionK: Arbitrary[Option ~> Option] =
    Arbitrary(Gen.oneOf(new (Option ~> Option) {
      def apply[A](fa: Option[A]): Option[A] = None
    }, FunctionK.id[Option]))

  implicit def eqKleisli[F[_], A, B](implicit arb: Arbitrary[A], ev: Eq[F[B]]): Eq[Kleisli[F, A, B]] = {
    Eq.by((x: (Kleisli[F, A, B])) => x.run)
  }

  implicit def stateTEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): Eq[StateT[F, S, A]] = {
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))
  }
}

class StoreTests extends StoreTestsBase {
  {
    implicit val monadLayerControl: MonadLayerControl.Aux[StateTC[Option, String]#l, Option, TupleC[String]#l] =
      cats.mtl.instances.statet.stateMonadLayerControl[Option, String]
    checkAll("StateT[Option, String, ?]",
      MonadLayerControlTests[StateTC[Option, String]#l, Option, TupleC[String]#l]
        .monadLayerControl[Boolean, Boolean])
    checkAll("MonadLayerControl[StateT[Option, String, ?], Option]",
      SerializableTests.serializable(monadLayerControl))
  }

  checkAll("State[String, String]",
    MonadStateTests[StateC[String]#l, String]
      .monadState[String])
  checkAll("MonadState[State[String, ?]]",
    SerializableTests.serializable(MonadState[StateC[String]#l, String]))
}
