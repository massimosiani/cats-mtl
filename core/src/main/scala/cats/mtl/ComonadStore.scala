package cats.mtl

import cats.{Comonad, Functor}

/**
 * The `ComonadStore[F, S]` stores a function together with an initial value, and lets
 * access and modify the initial value using `pos: S` and `seek(s: S): F[A]`
 */
trait ComonadStore[F[_], S] extends Serializable {
  val comonad: Comonad[F]

  def pos: S

  def peek[A](s: S): A

  def peeks[A](f: S => S): A

  def seek[A](s: S): F[A]

  def seeks[A](f: S => S): F[A]

  def experiment[G[_]: Functor, A](f: S => G[S]): G[A]
}

object ComonadStore {
  def pos[F[_], S](implicit ev: ComonadStore[F, S]): S = ev.pos

  def peek[F[_], S, A](s: S)(implicit ev: ComonadStore[F, S]): A = ev.peek(s)

  def peeks[F[_], S, A](f: S => S)(implicit ev: ComonadStore[F, S]): A = ev.peeks(f)

  def seek[F[_], S, A](s: S)(implicit ev: ComonadStore[F, S]): F[A] = ev.seek(s)

  def seeks[F[_], S, A](f: S => S)(implicit ev: ComonadStore[F, S]): F[A] = ev.seeks(f)

  def experiment[F[_], G[_]: Functor, S, A](f: S => G[S])(implicit ev: ComonadStore[F, S]): G[A] = ev.experiment(f)

  def apply[F[_], S](implicit ev: ComonadStore[F, S]): ComonadStore[F, S] = ev
}

trait DefaultComonadStore[F[_], S] extends ComonadStore[F, S] {

  def peeks[A](f: S => S): A = peek(f(pos))

  def experiment[G[_]: Functor, A](f: S => G[S]): G[A] = Functor[G].map(f(pos))(peek)
}
