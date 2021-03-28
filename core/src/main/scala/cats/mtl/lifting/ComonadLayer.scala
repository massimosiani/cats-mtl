package cats.mtl.lifting

import cats.Comonad

trait ComonadLayer[W[_], Inner[_]] extends FunctorLayer[W, Inner] {
  val outerInstance: Comonad[W]
  val innerInstance: Comonad[Inner]
}

object ComonadLayer {
  def apply[W[_], Inner[_]](implicit comonadLayer: ComonadLayer[W, Inner]): ComonadLayer[W, Inner] = comonadLayer
}
