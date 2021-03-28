package cats.mtl.laws

import cats.Comonad
import cats.laws.IsEq
import cats.mtl.ComonadStore
import cats.laws.IsEqArrow
import cats.syntax.comonad._
import cats.syntax.functor._
import cats.syntax.coflatMap._

trait ComonadStoreLaws[F[_], S] {
  implicit val storeInstance: ComonadStore[F, S]
  implicit val comonad: Comonad[F] = storeInstance.comonad

  import storeInstance._
  import comonad.extract
  import comonad.coflatMap

  // external laws:
  def posThenSeekDoesNothing[A](fa: F[A]): IsEq[F[A]] = {
    seek[A](pos) <-> fa
  }

  def seekThenPosDoesNothing[A](fs: F[S], s: S): IsEq[F[S]] = {
    seek[A](s).coflatMap(_ => pos) <-> fs
  }

  def seekThenSeekSeeksOnce[A](s1: S, s2: S): IsEq[F[A]] = {
    seek[A](s1).map(_ => seek[A](s2).extract) <-> seek(s2)
  }

  // internal law:
  def posIsCoflatMapThenPos[A, B](fa: F[A], f: F[A] => B): IsEq[S] = {
    pos <-> coflatMap(fa)(f).coflatMap(_ => pos).extract
  }

  def extractIsPeek[A](fa: F[A]): IsEq[A] = {
    extract(fa) <-> peek(pos)
  }
}

object ComonadStoreLaws {
  def apply[F[_], S](implicit instance0: ComonadStore[F, S]): ComonadStoreLaws[F, S] = new ComonadStoreLaws[F, S] {
    override lazy val storeInstance: ComonadStore[F, S] = instance0
  }
}
