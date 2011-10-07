package org.ulysses.data

import scalaz._
import Scalaz._

/**
 * A port of haskell's Data.Map based on a size balanced tree.
 * @see http://haskell.org/ghc/docs/latest/html/libraries/containers-0.4.1.0/Data-Map.html
 */
sealed trait BMap[K, V] {

  import BMap._

  type ShowS = String => String

  def showString(s: String): String = s

  def fold[R](f: Bin[K, V] => R, g: => R) = this match {
    case Tip() => g
    case b@Bin(_, _, _, _, _) => f(b)
  }

  def isEmpty: Boolean = fold(_ => false, true)

  def size: Int = fold(_.s, 0)

  def lookup(key: K)(implicit o: Order[K]): Option[V] = this match {
    case Tip() => None
    case Bin(_, k, v, l, r) => {
      if (o.isLT(key)(k)) l.lookup(key)(o)
      else if (o.isGT(key)(k)) r.lookup(key)(o)
      else Some(v)
    }
  }

  def lookupAssoc(key: K)(implicit o: Order[K]): Option[(K, V)] = this match {
    case Tip() => None
    case Bin(_, k, v, l, r) => {
      if (o.isLT(key)(k)) l.lookupAssoc(key)(o)
      else if (o.isGT(key)(k)) r.lookupAssoc(key)(o)
      else Some((k, v))
    }
  }

  def member(k: K)(implicit o: Order[K]): Boolean = lookup(k)(o).fold(_ => true, false)

  def notMember(k: K)(implicit o: Order[K]): Boolean = !member(k)(o)

  def find(k: K)(implicit o: Order[K]): V = lookup(k)(o).fold(identity, sys.error("BMap.find: element not in the map"))

  def findWithDefault(v: V, k: K)(implicit o: Order[K]): V = lookup(k)(o).fold(identity, v)

  def insert(kx: K, x: V)(implicit o: Order[K]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => singleton(kx, x)
      case Bin(sy, ky, y, l, r) => {
        if (o.isLT(kx)(ky)) balance(ky, y, (go(l)), r)
        else if (o.isGT(kx)(ky)) balance(ky, y, l, (go(r)))
        else Bin(sy, kx, x, l, r)
      }
    }
    go(this)
  }

  def insertWith(fx: V => V => V, kx: K, x: V)(implicit o: Order[K]): BMap[K, V] = insertWithKey(_ => fx, kx, x)(o)

  def insertWithKey(f: K => V => V => V, kx: K, x: V)(implicit o: Order[K]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => singleton(kx, x)
      case Bin(sy, ky, y, l, r) => {
        if (o.isLT(kx)(ky)) balance(ky, y, (go(l)), r)
        else if (o.isGT(kx)(ky)) balance(ky, y, l, (go(r)))
        else Bin(sy, kx, f(kx)(x)(y), l, r)
      }
    }
    go(this)
  }

  def insertLookupWithKey(f: K => V => V => V, kx: K, x: V)(implicit o: Order[K]): (Option[V], BMap[K, V]) = {
    def go(m: BMap[K, V]): (Option[V], BMap[K, V]) = m match {
      case Tip() => (None, singleton(kx, x))
      case Bin(sy, ky, y, l, r) => {
        if (o.isLT(kx)(ky)) {
          val (found, gol) = go(l)
          (found, balance(ky, y, gol, r))
        }
        else if (o.isGT(kx)(ky)) {
          val (found, gor) = go(r)
          (found, balance(ky, y, l, gor))
        }
        else (Some(y), Bin(sy, kx, f(kx)(x)(y), l, r))
      }
    }

    go(this)
  }

  def delete(k: K)(implicit o: Order[K]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => Tip()
      case Bin(_, kx, x, l, r) => {
        if (o.isLT(k)(kx)) balance(kx, x, (go(l)), r)
        else if (o.isGT(k)(kx)) balance(kx, x, l, (go(r)))
        else glue(l, r)
      }
    }
    go(this)
  }

  def glue(left: BMap[K, V], right: BMap[K, V]): BMap[K, V] = (left, right) match {
    case (Tip(), r) => r
    case (l, Tip()) => l
    case (l, r) => {
      if (l.size > r.size) {
        val ((km, m), ll) = deleteFindMaxWith(l)
        balance(km, m, ll, r)
      }
      else {
        val ((km, m), rr) = deleteFindMinWith(r)
        balance(km, m, l, rr)
      }
    }
  }

  def deleteFindMin: ((K, V), BMap[K, V]) = deleteFindMinWith(this)

  private def deleteFindMinWith(m: BMap[K, V]): ((K, V), BMap[K, V]) = m match {
    case Bin(_, k, x, Tip(), r) => ((k, x), r)
    case Bin(_, k, x, l, r) => {
      val (km, ll) = deleteFindMinWith(l)
      (km, balance(k, x, ll, r))
    }
    case Tip() => sys.error("Map.deleteFindMin: can not return the minimal element of an empty map Tip")
  }

  def deleteFindMax: ((K, V), BMap[K, V]) = deleteFindMaxWith(this)

  private def deleteFindMaxWith(m: BMap[K, V]): ((K, V), BMap[K, V]) = m match {
    case Bin(_, k, x, l, Tip()) => ((k, x), l)
    case Bin(_, k, x, l, r) => {
      val (km, rr) = deleteFindMaxWith(r)
      (km, balance(k, x, l, rr))
    }
    case Tip() => sys.error("Map.deleteFindMax: can not return the minimal element of an empty map Tip")
  }

  def updateMin(f: V => Option[V]): BMap[K, V] = {
    updateMinWithKey(_ => f)
  }

  def updateMinWithKey(f: K => V => Option[V]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => Tip()
      case Bin(sx, kx, x, Tip(), r) => f(kx)(x) match {
        case None => r
        case Some(xl) => Bin(sx, kx, xl, Tip(), r)
      }
      case Bin(_, kx, x, l, r) => balance(kx, x, go(l), r)
    }
    go(this)
  }

  def updateMax(f: V => Option[V]): BMap[K, V] = {
    updateMaxWithKey(_ => f)
  }

  def updateMaxWithKey(f: K => V => Option[V]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => Tip()
      case Bin(sx, kx, x, l, Tip()) => f(kx)(x) match {
        case None => l
        case Some(xl) => Bin(sx, kx, xl, l, Tip())
      }
      case Bin(_, kx, x, l, r) => balance(kx, x, l, go(r))
    }
    go(this)
  }

  def minViewWithKey: Option[((K, V), BMap[K, V])] = this match {
    case Tip() => None
    case _ => Some(deleteFindMin)
  }

  def maxViewWithKey: Option[((K, V), BMap[K, V])] = this match {
    case Tip() => None
    case _ => Some(deleteFindMax)
  }

  def minView: Option[(V, BMap[K, V])] = this match {
    case Tip() => None
    case _ => {
      val ((kx, x), m) = deleteFindMin
      Some(x, m)
    }
  }

  def maxView: Option[(V, BMap[K, V])] = this match {
    case Tip() => None
    case _ => {
      val ((kx, x), m) = deleteFindMax
      Some(x, m)
    }
  }

  def union(m: BMap[K, V])(implicit o: Order[K]): BMap[K, V] = (m, this) match {
    case (Tip(), t2) => t2
    case (t1, Tip()) => t1
    case (t1, t2) => hedgeUnionL(_ => LT, _ => GT, t1, t2)
  }


  def hedgeUnionL(k1: K => Ordering, k2: K => Ordering, m1: BMap[K, V], m2: BMap[K, V])(implicit o: Order[K]): BMap[K,V] = (k1, k2, m1, m2) match {
    case (_, _,t1, Tip()) => t1
    case (cmplo, cmphi, Tip(), Bin(_, kx, x, l, r)) => join(kx, x, filterGt(cmplo, l), filterLt(cmphi, r))
    case (cmplo, cmphi, Bin(_, kx, x, l, r), t2) => {
      val cmpkx: K => Ordering = k => o.order(kx)(k)
      join(kx, x, hedgeUnionL(cmplo, cmpkx, l, trim(cmplo, cmpkx, t2)), hedgeUnionL(cmpkx, cmphi, r, trim(cmpkx, cmphi, t2)))
    }
  }


//unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
//unionWithKey _ Tip t2  = t2
//unionWithKey _ t1 Tip  = t1
//unionWithKey f t1 t2 = hedgeUnionWithKey f (const LT) (const GT) t1 t2
//
//hedgeUnionWithKey :: Ord a
//                  => (a -> b -> b -> b)
//                  -> (a -> Ordering) -> (a -> Ordering)
//                  -> Map a b -> Map a b
//                  -> Map a b

  def hedgeUnionWithKey(k1: K => V => V => V)(k2: K => Ordering)(k3: K => Ordering)(m1: BMap[K, V])(m2: BMap[K,V])(implicit o: Order[K]) : BMap[K, V] =
    (k1, k2, k3, m1, m2) match {
      case (_, _, _, t1, Tip()) => t1
      case (_, cmplo, cmphi, Tip(), Bin(_, kx, x, l, r)) => join(kx, x, (filterGt(cmplo, l)), filterLt(cmphi, r))
      case (f, cmplo, cmphi, Bin(_, kx, x, l, r), t2) => {
        val cmpkx = o.order(kx)
        val lt = trim(cmplo, cmpkx, t2)
        val (found, gt) = trimLookupLo(kx, cmphi, t2)
        val newx = found.fold(xy => f(kx)(x)(xy._2), x)
        join(kx, newx, hedgeUnionWithKey(f)(cmplo)(cmpkx)(l)(lt), hedgeUnionWithKey(f)(cmpkx)(cmphi)(r)(gt))
    }
  }

  def trim(k1: K => Ordering, k2: K => Ordering, m: BMap[K, V]): BMap[K, V] = (k1, k2, m) match {
    case (_, _, Tip()) => Tip()
    case (cmplo, cmphi, t@Bin(_, kx, _, l, r)) => cmplo(kx) match {
      case LT => cmphi(kx) match {
        case GT => t
        case _ => trim(cmplo, cmphi, l)
      }
      case _ => trim(cmplo, cmphi, r)
    }
  }

  def trimLookupLo(k1: K, k2: K => Ordering, m: BMap[K, V])(implicit o: Order[K]): (Option[(K, V)], BMap[K, V]) = (k1, k2, m) match {
    case (_, _, Tip()) => (None, Tip())
    case (lo, cmphi, t@Bin(_, kx, x, l, r)) => o.order(lo)(kx) match {
      case LT => cmphi(kx) match {
        case GT => (t.lookupAssoc(lo), t)
        case _ => trimLookupLo(lo, cmphi, l)
      }
      case GT => trimLookupLo(lo, cmphi, r)
      case EQ => (Some(kx, x), trim(o.order(lo), cmphi, r))
    }
  }

  def join(kx: K, x: V, m1: BMap[K, V], m2: BMap[K, V])(implicit o: Order[K]): BMap[K, V] = (m1, m2) match {
    case (Tip(), r) => insertMin(kx, x, r)
    case (l, Tip()) => insertMax(kx, x, l)
    case (l@Bin(sizeL, ky, y, ly, ry), r@Bin(sizeR, kz, z, lz, rz)) => {
      if (delta*sizeL <= sizeR) balance(kz, z, join(kx, x, l, lz), rz)
      else if (delta*sizeR <= sizeL) balance(ky, y, ly, join(kx, x, ry, r))
      else bin(kx, x, l, r)
    }
  }

  def insertMin(kx: K, x: V, m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => singleton(kx, x)
      case Bin(_, ky, y, l, r) => balance(ky, y, (insertMin(kx, x, l)), r)
  }

  def insertMax(kx: K, x: V, m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => singleton(kx, x)
      case Bin(_, ky, y, l, r) => balance(ky, y, l, (insertMax(kx, x, r)))
  }

  def filterGt(cmp: K => Ordering, m: BMap[K, V])(implicit o: Order[K]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => Tip()
      case Bin(_, kx, x, l, r) => cmp(kx) match {
        case LT => join(kx, x, go(l), r)
        case GT => go(r)
        case EQ => r
      }
    }
    go(m)
  }

  def filterLt(cmp: K => Ordering, m: BMap[K, V])(implicit o: Order[K]): BMap[K, V] = {
    def go(m: BMap[K, V]): BMap[K, V] = m match {
      case Tip() => Tip()
      case Bin(_, kx, x, l, r) => cmp(kx) match {
        case LT => go(l) join(kx, x, go(l), r)
        case GT => join(kx, x, l, go(r))
        case EQ => l
      }
    }
    go(m)
  }

  def map[B](f: V => B): BMap[K, B] = mapWithKey(_ => f)

  def mapWithKey[B](f: K => V => B): BMap[K, B] = {
    def go(m: BMap[K, V]): BMap[K, B] = m match {
      case Tip() => Tip()
      case Bin(sx, kx, x, l, r) => Bin(sx, kx, f(kx)(x), go(l), go(r))
    }
    go(this)
  }

  def showTree: String = showTreeWith(k => v => k.toString ++ ":=" ++ v.toString, true, true)

  def showTreeWith(showelem: K => V => String, hang: Boolean, wide: Boolean): String = {
    if (hang) showsTreeHang(showelem, wide, List(), this)
    else showsTreeHang(showelem, wide, List(), this)
  }

  def showsTree(showelem: K => V => String, wide: Boolean, lbars: List[String], rbars: List[String], t: BMap[K, V]): String = t match {
    case Tip() => showsBars(lbars)
    case _ => ""
  }

  def showsTreeHang(showelem: K => V => String, wide: Boolean, bars: List[String], t: BMap[K, V]): String = t match {
    case Tip() => showsBars(bars) ++ "|\n"
    case Bin(_, kx, x, Tip(), Tip()) => showsBars(bars) ++ showelem(kx)(x) ++ "\n"
    case Bin(_, kx, x, l, r) => {
      //TODO there must be a better way to do this...
      showsBars(bars) ++ showelem(kx)(x) ++ "\n" ++ showWide(wide, bars) ++ showsTreeHang(showelem, wide, withBar(bars), l) ++ showWide(wide, bars) ++ showsTreeHang(showelem, wide, (withEmpty(bars)), r)
    }
  }

  private def node: String = "+--"

  private def showWide(wide: Boolean, bars: List[String]): String = {
    if (wide) bars.reverse.mkString + "|\n"
    else bars.mkString
  }

  private def showsBars(bars: List[String]): String = bars match {
    case List() => ""
    case _ => bars.tail.reverse.mkString ++ node
  }

  private def withBar(bars: List[String]): List[String] = "|  " :: bars

  private def withEmpty(bars: List[String]): List[String] = "  " :: bars


  private def balance(k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = {
    val sizeL = l.size
    val sizeR = r.size
    val sizeX = sizeL + sizeR + 1
    println("balance: sizeL %s sizeR %s".format(sizeL, sizeR))
    if (sizeL + sizeR <= 1) Bin(sizeX, k, v, l, r)
    else if (sizeR >= delta * sizeL) rotateL(k, v, l, r)
    else if (sizeL >= delta * sizeR) rotateR(k, v, l, r)
    else Bin(sizeX, k, v, l, r)
  }

  private def rotateL(k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = r match {
    case Bin(_, _, _, ly, ry) if (ly.size < ratio * ry.size) => singleL(k, v, l, r)
    case Bin(_, _, _, ly, ry) => doubleL(k, v, l, r)
    case Tip() => sys.error("rotateL tip")
  }

  private def singleL(k1: K, v1: V, l1: BMap[K, V], r2: BMap[K, V]): BMap[K, V] = r2 match {
    case Bin(_, k2, v2, l2, r2) => bin(k2, v2, bin(k1, v1, l1, l2), r2)
    case Tip() => sys.error("singleL Tip")
  }

  private def doubleL(k1: K, x1: V, t1: BMap[K, V], b: BMap[K, V]): BMap[K, V] = b match {
    case Bin(_, k2, x2, Bin(_, k3, x3, t2, t3), t4) => bin(k3, x3, bin(k1, x1, t1, t2), bin(k2, x2, t3, t4))
    case _ => sys.error("doubleL")
  }

  private def rotateR(k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = l match {
    case Bin(_, _, _, ly, ry) if (ry.size < ratio * ly.size) => singleR(k, v, l, r)
    case Bin(_, _, _, ly, ry) => doubleR(k, v, l, r)
    case Tip() => sys.error("rotateL tip")
  }

  private def singleR(k1: K, x1: V, b: BMap[K, V], t3: BMap[K, V]): BMap[K, V] = b match {
    case Bin(_, k2, x2, t1, t2) => bin(k2, x2, t1, bin(k1, x1, t2, t3))
    case Tip() => sys.error("singleR Tip")
  }

  private def doubleR(k1: K, x1: V, b: BMap[K, V], t4: BMap[K, V]): BMap[K, V] = b match {
    case Bin(_, k2, x2, t1, Bin(_, k3, x3, t2, t3)) => bin(k3, x3, bin(k2, x2, t1, t2), bin(k1, x1, t3, t4))
    case _ => sys.error("doubleR")
  }
}


object BMap {

  val delta = 4
  val ratio = 2

  def singleton[K, V](k: K, v: V): BMap[K, V] = Bin(1, k, v, Tip(), Tip())

  def empty[K, V]: BMap[K, V] = Tip()

  def bin[K, V](k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = Bin(l.size + r.size + 1, k, v, l, r)

  case class Tip[K, V]() extends BMap[K, V]

  case class Bin[K, V](s: Int, k: K, v: V, left: BMap[K, V], right: BMap[K, V]) extends BMap[K, V]

//  implicit def MapShow[K: Show, V: Show]: Show[BMap[K, V]] =
//    Show.show((m: BMap[K, V]) => ""
//      '{' :: implicitly[Show[A]].show(t.rootLabel) ++ " " ++ implicitly[Show[Stream[Tree[A]]]].show(t.subForest) ++ "}")

   implicit def BMapMonoid[K, V](implicit ss: Semigroup[V]): Monoid[Map[K,V]] = Monoid.monoid

   implicit def BMapFunctor[K]: Functor[({type λ[α] = BMap[K, α]})#λ] = new Functor[({type λ[α] = BMap[K, α]})#λ] {
     def fmap[A, B](f: A => B) = _ map f
   }

//instance (Show k, Show a) => Show (Map k a) where
//  showsPrec d m  = showParen (d > 10) $
//    showString "fromList " . shows (toList m)
}


