package cats.mtl.lifting

trait ComonadLayerFunctor[W[_], Inner[_]] extends ComonadLayer[W, Inner] with FunctorLayerFunctor[W, Inner]

object ComonadLayerFunctor {
  def apply[W[_], Inner[_]](implicit comonadLayerFunctor: ComonadLayerFunctor[W, Inner]): ComonadLayerFunctor[W, Inner] = comonadLayerFunctor
}
