package cats.mtl.instances

import cats.{Comonad, Functor}
import cats.mtl.ComonadStore
import cats.mtl.lifting.ComonadLayerFunctor

trait StoreInstances {
  implicit final def storeInd[W[_], Inner[_], E](implicit cl: ComonadLayerFunctor[W, Inner], under: ComonadStore[Inner, E]): ComonadStore[W, E] = {
    new ComonadStore[W, E] {
      val comonad: Comonad[W] = cl.outerInstance

      override def pos: E = under.pos

      override def peek[A](s: E): A = under.peek(s)

      override def peeks[A](f: E => E): A = under.peeks(f)

      override def seek[A](s: E): W[A] = cl.layer(under.seek(s))

      override def seeks[A](f: E => E): W[A] = cl.layer(under.seeks(f))

      override def experiment[G[_] : Functor, A](f: E => G[E]): G[A] = under.experiment(f)
    }
  }
}

object store extends StoreInstances
