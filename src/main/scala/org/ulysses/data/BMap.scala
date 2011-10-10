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

  def fold[R](empty: => R, nonempty: (Int, K, V, BMap[K, V], BMap[K, V]) => R): R

  /**
   * O(1). Is the map empty?
   * empty.isEmpty => true
   * singleton(1, ""a").isEmpty => false
   */
  def isEmpty: Boolean = size == 0

  /**
   * O(1) the number of elements in the map
   */
  def size: Int = fold(0, (s, _, _, _, _) => s)

  /**
   * O(log(n)) lookup the value with the corresponding key in the map.
   */
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

  /**
   * Finds the value associated with the given key K in the map. Throws an exception if the key is not present.
   */
  def find(k: K)(implicit o: Order[K]): V = lookup(k)(o).fold(identity, sys.error("BMap.find: element not in the map"))

  /**
   * O(log n)
   * Finds the value associated with the given key K in the map, returns the given default value if the key is not present.
   */
  def findWithDefault(v: V, k: K)(implicit o: Order[K]): V = lookup(k)(o).fold(identity, v)

  /**
   * O(log n). Inserts the given key value pair in the map. If the key is already present, the value associated with
   * the key will be overwritten with the new value.
   */
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
        if (o.isLT(kx)(ky)) balance(ky, y, go(l), r)
        else if (o.isGT(kx)(ky)) balance(ky, y, l, go(r))
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
      case Tip() => Tip[K, V]
      case Bin(_, kx, x, l, r) => {
        if (o.isLT(k)(kx)) balance(kx, x, (go(l)), r)
        else if (o.isGT(k)(kx)) balance(kx, x, l, (go(r)))
        else glue(l, r)
      }
    }
    go(this)
  }


  def adjust(v: V => V, k: K)(implicit o: Order[K]): BMap[K, V] = adjustWithKey(_ => v)(k)

  /**
   * O(log n). Adjust a value with a specific key. When the key is not a memeber of the map,
   * the original map is returned.
   */
  def adjustWithKey(f: K => V => V)(k: K)(implicit o: Order[K]): BMap[K, V] = updateWithKey(k1 => v1 => Some(f(k1)(v1)))(k)

  /**
   * O(log n)/. The expression update(f, k) updates the value x if the given key is in the map.
   * If f(v) is None, the element is deleted. If it is Some(y) the key k is bound to the new value y.
   */
  def update(f: V => Option[V], k: K)(implicit o: Order[K]) : BMap[K, V] = updateWithKey(_ => x => f(x))(k)
  
  /**
   * O(log n). The expression ('updateWithKey' f k map) updates the
   * value v at k (if it is in the map). If (f k v) is 'None',
   * the element is deleted. If it is Some(y) the key k is bound to the new value y.
   */
  def updateWithKey(f: K => V => Option[V])(k: K)(implicit o: Order[K]): BMap[K, V] = {
    def go(m : BMap[K, V]): BMap[K, V] = m match {
      case Tip() => empty[K, V]
      case Bin(sx, kx, x, l, r) => o.order(k)(kx) match {
        case LT => balance(kx, x, go(l), r)
        case GT => balance(kx, x, l, go(r))
        case EQ => f(kx)(x).fold(xx => Bin(sx, kx, xx, l, r), glue(l, r))
      }
    }
    go(this)
  }

  /**
   * O (log n). Lookup and update.
   * The function returns changed value, if it is updated.
   * Returns the original key value if the map entry is deleted.
   * @see updateWithKey
   */
  def updateLookupWithKey(f: K => V => Option[V])(k: K)(implicit o: Order[K]): (Option[V], BMap[K, V]) = {
    def go(m : BMap[K, V]): (Option[V], BMap[K, V]) = m match {
      case Tip() => (None, empty[K, V])
      case Bin(sx, kx, x, l, r) => o.order(k)(kx) match {
        case LT => {
          val (found, l1) = go(l)
          (found, balance(kx, x, l1, r))
        }
        case GT => {
          val (found, r1) = go(r)
          (found, balance(kx, x, l, r1))
        }
        case EQ => (Some(x), glue(l, r))
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
      case Tip() => empty
      case Bin(sx, kx, x, Tip(), r) => f(kx)(x) match {
        case None => r
        case Some(xl) => Bin(sx, kx, xl, empty, r)
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
      case Tip() => empty
      case Bin(sx, kx, x, l, Tip()) => f(kx)(x) match {
        case None => l
        case Some(xl) => Bin(sx, kx, xl, l, empty)
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

  /**
   * O(n+m).
   * The expression t1.union(t2) takes the left-biased union of t1 and t2, preferring t1 when duplicate keys are encountered.
   * The implementation uses the efficient hedge-union algorithm.
   * Hedge-union is more efficient on (bigset \``union`\` smallset).
   */
  def union(m: BMap[K, V])(implicit o: Order[K]): BMap[K, V] = (this, m) match {
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
    case (_, _, Tip()) => empty
    case (cmplo, cmphi, t@Bin(_, kx, _, l, r)) => cmplo(kx) match {
      case LT => cmphi(kx) match {
        case GT => t
        case _ => trim(cmplo, cmphi, l)
      }
      case _ => trim(cmplo, cmphi, r)
    }
  }

  def trimLookupLo(k1: K, k2: K => Ordering, m: BMap[K, V])(implicit o: Order[K]): (Option[(K, V)], BMap[K, V]) = (k1, k2, m) match {
    case (_, _, Tip()) => (None, empty)
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
      case Tip() => empty
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
      case Tip() => empty
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
      case Tip() => empty
      case Bin(sx, kx, x, l, r) => Bin(sx, kx, f(kx)(x), go(l), go(r))
    }
    go(this)
  }



//  {--------------------------------------------------------------------
//  Lists
//  use [foldlStrict] to reduce demand on the control-stack
//--------------------------------------------------------------------}
//-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
//-- If the list contains more than one value for the same key, the last value
//-- for the key is retained.
//--
//-- > fromList [] == empty
//-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
//-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
//
//fromList :: Ord k => [(k,a)] -> Map k a
//fromList xs
//  = foldlStrict ins empty xs
//  where
//    ins t (k,x) = insert k x t

//  -- | /O(n)/. Fold the values in the map, such that
//-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
//-- For example,
//--
//-- > elems map = fold (:) [] map
//--
//-- > let f a len = len + (length a)
//-- > fold f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
//fold :: (a -> b -> b) -> b -> Map k a -> b
//fold f = foldWithKey (\_ x' z' -> f x' z')

  def fold[B](f: V => B => B)(b: B): B = foldrWithKey(_ => f)(b)

  def foldrWithKey[B](f: K => V => B => B)(b: B): B = {
    def go(z: B, m: BMap[K, V]): B = (z, m) match {
      case (z, Tip()) => z
      case (z, Bin(_, kx, x, l, r)) => go(f(kx)(x)(go(z, r)), l)
    }
    go(b, this)
  }

  /**
   * O(n). Pre-order fold. The function will be applied from the highest value to the lowest. 
   */
  def foldlWithKey[B](f: B => K => V => B)(b: B): B = {
    def go(z: B, m: BMap[K, V]): B = (z, m) match {
      case (z, Tip()) => z
      case (z, Bin(_, kx, x, l, r)) => go(f(go(z, l))(kx)(x), r)
    }
    go(b, this)
  }

  def toList: List[(K, V)] = toAscList

  def toAscList: List[(K, V)] = foldrWithKey[List[(K, V)]](k => x => xs => (k, x) :: xs)(Nil)

  def toDescList: List[(K, V)] = foldlWithKey[List[(K, V)]](xs => k => x => (k, x) :: xs)(Nil)

  //debugging /- printing the map
  def showTree: String = showTreeWith(k => v => k.toString ++ ":=" ++ v.toString, true, true)

  def showTreeWith(showelem: K => V => String, hang: Boolean, wide: Boolean): String = {
    if (hang) showsTreeHang(showelem, wide, List(), this)
    else showsTreeHang(showelem, wide, List(), this)//TODO obviously...
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

  private lazy val node: String = "+--"

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

trait BMaps {
  import BMap._
  def fromList[K, V](l: List[(K, V)])(implicit o: Order[K]): BMap[K, V] = l.foldLeft(empty[K, V]){(t, kv) => t.insert(kv._1, kv._2)}
}

object BMap extends BMaps {

  val delta = 4
  val ratio = 2

  def singleton[K, V](k: K, v: V): BMap[K, V] = Bin(1, k, v, empty, empty)

  def empty[K, V]: BMap[K, V] = Tip[K, V]

  /**The empty heap */
  object Tip {
    def apply[K, V]: BMap[K, V] = new BMap[K, V] {
      def fold[R](empty: => R, nonempty: (Int, K, V, BMap[K, V], BMap[K, V]) => R) = empty
    }

    def unapply[K, V](m: BMap[K, V]): Boolean = m.fold(true, (_, _, _, _, _) => false)
  }

  def bin[K, V](k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = Bin(l.size + r.size + 1, k, v, l, r)

//  case class Tip[K, V]() extends BMap[K, V]

  case class Bin[K, V](s: Int, k: K, v: V, left: BMap[K, V], right: BMap[K, V]) extends BMap[K, V] {
    def fold[R](empty: => R, nonempty: (Int, K, V, BMap[K, V], BMap[K, V]) => R) = nonempty(s, k, v, left, right)
  }

//  implicit def MapShow[K: Show, V: Show]: Show[BMap[K, V]] =
//    Show.show((m: BMap[K, V]) => ""
//      '{' :: implicitly[Show[A]].show(t.rootLabel) ++ " " ++ implicitly[Show[Stream[Tree[A]]]].show(t.subForest) ++ "}")

   implicit def BMapMonoid[K, V](implicit ss: Semigroup[V]): Monoid[Map[K,V]] = Monoid.monoid

   implicit def BMapFunctor[K]: Functor[({type λ[α] = BMap[K, α]})#λ] = new Functor[({type λ[α] = BMap[K, α]})#λ] {
     def fmap[A, B](f: A => B) = _ map f
   }

  implicit def BMapEqual[K: Equal, V: Equal]: Equal[BMap[K, V]] =
    Equal.equalC[BMap[K, V]]((t1, t2) => t1.size == t2.size && (t1.toList == t2.toList))


//instance (Show k, Show a) => Show (Map k a) where
//  showsPrec d m  = showParen (d > 10) $
//    showString "fromList " . shows (toList m)
}


