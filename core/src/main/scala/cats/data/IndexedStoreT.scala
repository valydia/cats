package cats
package data

class IndexedStoreTRepresentable[F[_], G[_]: Representable, I, A](val run: (F[G[A]], I)) extends Serializable {

  import IndexedStoreTRepresentable._

  val FunctorG = Representable[G].F

  def imap[X](f: I => X): IndexedStoreTRepresentable[F, G, X, A] =
    indexedStoreTRepresentable((set, f(pos)))

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedStoreTRepresentable[F, G, I, B] = {
    indexedStoreTRepresentable( (F.compose[G](FunctorG).map(set)(f), pos))
  }

  def bimap[X, Y](f: I => X, g: A => Y)(implicit F: Functor[F]): IndexedStoreTRepresentable[F, G, X, Y] =
    indexedStoreTRepresentable((F.compose[G](FunctorG).map(set)(g), f(pos)))

  def leftMap[X](f: I => X): IndexedStoreTRepresentable[F, G, X, A]  =
    imap(f)

  // What's a correct signature for coflatmap?
  def coflatMap[K, C](f: IndexedStoreTRepresentable[F, G, K, A] => C)(implicit F: CoflatMap[F]): IndexedStoreTRepresentable[F, G, C, I] = {
    // How to get a CoflatMap for F[G[_]] ???
    ???
  }


  def set: F[G[A]] = run._1

  def pos: I = run._2

}

object IndexedStoreTRepresentable extends CommonStoreTRepresentableConstructors

private[data] trait CommonStoreTRepresentableConstructors {

  def indexedStoreTRepresentable[F[_], G[_]: Representable, I, B](r: (F[G[B]], I)): IndexedStoreTRepresentable[F, G, I, B] =
    new IndexedStoreTRepresentable(r)

}

import cats.instances.function._

final class IndexedStoreT[F[_], I, A, B](override val run: (F[A => B], I)) extends IndexedStoreTRepresentable[F, A => ?, I, B](run) {

  import IndexedStoreT._

  //
   def coflatMap[K, C](f: IndexedStoreT[F, K, A, B] => C)(implicit F: CoflatMap[F]): IndexedStoreT[F, I, K, C] = {

    indexedStoreT((F.coflatMap(run._1)(ff => (a:K) => f(indexedStoreT((ff, a)))), pos))
  }
}

object IndexedStoreT extends CommonStoreTConstructors

private[data] trait CommonStoreTConstructors {

  def indexedStoreT[F[_], I, A, B](r: (F[A => B], I)): IndexedStoreT[F, I, A, B] =
    new IndexedStoreT(r)


}


