package org.ulysses.data

import scalaz._
//import Scalaz._

/**
 * A port of haskell's Data.Map based on a size balanced tree.
 * @see http://haskell.org/ghc/docs/latest/html/libraries/containers-0.4.1.0/Data-Map.html
 */
sealed trait BMap[K, A] {

  import BMap._

  type ShowS = String => String

  def showString(s: String): String = s

  def fold[R](empty: => R, nonempty: (Int, K, A, BMap[K, A], BMap[K, A]) => R): R

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
  def lookup(key: K)(implicit o: Order[K]): Option[A] = this match {
    case Tip() => None
    case Bin(_, k, v, l, r) => {
      if (o.lessThan(key, k)) l.lookup(key)(o)
      else if (o.greaterThan(key, k)) r.lookup(key)(o)
      else Some(v)
    }
  }

  def lookupAssoc(key: K)(implicit o: Order[K]): Option[(K, A)] = this match {
    case Tip() => None
    case Bin(_, k, v, l, r) => {
      if (o.lessThan(key, k)) l.lookupAssoc(key)(o)
      else if (o.greaterThan(key, k)) r.lookupAssoc(key)(o)
      else Some((k, v))
    }
  }

  def member(k: K)(implicit o: Order[K]): Boolean = lookup(k)(o) match {
    case Some(x) => true
    case None => false
  }

  def notMember(k: K)(implicit o: Order[K]): Boolean = !member(k)

  /**
   * Finds the value associated with the given key K in the map. Throws an exception if the key is not present.
   */
  def find(k: K)(implicit o: Order[K]): A = lookup(k) match {
    case Some(x) => x
    case None => sys.error("BMap.find: element not in the map")
  }

  /**
   * O(log n)
   * Finds the value associated with the given key K in the map, returns the given default value if the key is not present.
   */
  def findWithDefault(v: A, k: K)(implicit o: Order[K]): A = lookup(k)(o) match {
    case Some(x) => x
    case None => v
  }

  /**
   * O(log n). Inserts the given key value pair in the map. If the key is already present, the value associated with
   * the key will be overwritten with the new value.
   */
  def insert(kx: K, x: A)(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => singleton(kx, x)
      case Bin(sy, ky, y, l, r) => {
        if (o.lessThan(kx, ky)) balance(ky, y, (go(l)), r)
        else if (o.greaterThan(kx, ky)) balance(ky, y, l, (go(r)))
        else Bin(sy, kx, x, l, r)
      }
    }
    go(this)
  }

  def insertWith(fx: A => A => A, kx: K, x: A)(implicit o: Order[K]): BMap[K, A] = insertWithKey(_ => fx, kx, x)(o)

  def insertWithKey(f: K => A => A => A, kx: K, x: A)(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => singleton(kx, x)
      case Bin(sy, ky, y, l, r) => {
        if (o.lessThan(kx, ky)) balance(ky, y, go(l), r)
        else if (o.greaterThan(kx, ky)) balance(ky, y, l, go(r))
        else Bin(sy, kx, f(kx)(x)(y), l, r)
      }
    }
    go(this)
  }

  def insertLookupWithKey(f: K => A => A => A, kx: K, x: A)(implicit o: Order[K]): (Option[A], BMap[K, A]) = {
    def go(m: BMap[K, A]): (Option[A], BMap[K, A]) = m match {
      case Tip() => (None, singleton(kx, x))
      case Bin(sy, ky, y, l, r) => {
        if (o.lessThan(kx, ky)) {
          val (found, gol) = go(l)
          (found, balance(ky, y, gol, r))
        }
        else if (o.greaterThan(kx, ky)) {
          val (found, gor) = go(r)
          (found, balance(ky, y, l, gor))
        }
        else (Some(y), Bin(sy, kx, f(kx)(x)(y), l, r))
      }
    }

    go(this)
  }

  def delete(k: K)(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => Tip[K, A]
      case Bin(_, kx, x, l, r) => {
        if (o.lessThan(k, kx)) balance(kx, x, (go(l)), r)
        else if (o.greaterThan(k, kx)) balance(kx, x, l, (go(r)))
        else glue(l, r)
      }
    }
    go(this)
  }


  def adjust(v: A => A, k: K)(implicit o: Order[K]): BMap[K, A] = adjustWithKey(_ => v)(k)

  /**
   * O(log n). Adjust a value with a specific key. When the key is not a memeber of the map,
   * the original map is returned.
   */
  def adjustWithKey(f: K => A => A)(k: K)(implicit o: Order[K]): BMap[K, A] = updateWithKey(k1 => v1 => Some(f(k1)(v1)))(k)

  /**
   * O(log n)/. The expression update(f, k) updates the value x if the given key is in the map.
   * If f(v) is None, the element is deleted. If it is Some(y) the key k is bound to the new value y.
   */
  def update(f: A => Option[A], k: K)(implicit o: Order[K]): BMap[K, A] = updateWithKey(_ => x => f(x))(k)

  /**
   * O(log n). The expression ('updateWithKey' f k map) updates the
   * value v at k (if it is in the map). If (f k v) is 'None',
   * the element is deleted. If it is Some(y) the key k is bound to the new value y.
   */
  def updateWithKey(f: K => A => Option[A])(k: K)(implicit o: Order[K]): BMap[K, A] = {
    import std.Option.option._
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => BMap.empty[K, A]
      case Bin(sx, kx, x, l, r) => {
          if (o.lessThan(k, kx)) balance(kx, x, go(l), r)
          else if (o.greaterThan(k, kx)) balance(kx, x, l, go(r))
          else f(kx)(x) match {
             case Some(xx) => Bin(sx, kx, xx, l, r)
             case None => glue(l, r)
          }
        }
      }
    go(this)
  }

  import scalaz.Ordering._
  /**
   * O (log n). Lookup and update.
   * The function returns changed value, if it is updated.
   * Returns the original key value if the map entry is deleted.
   * @see updateWithKey
   */
  def updateLookupWithKey(f: K => A => Option[A])(k: K)(implicit o: Order[K]): (Option[A], BMap[K, A]) = {
    def go(m: BMap[K, A]): (Option[A], BMap[K, A]) = m match {
      case Tip() => (None, empty[K, A])
      case Bin(sx, kx, x, l, r) => o.order(k, kx) match {
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

  /**
   * O(log n). The expression m.alter(f)(k) alters the value x at k, or absence thereof.
   * 'alter' can be used to insert, delete or update a value in a Map.
   * In short: m lookup(k)(alter(f)(k) = f(m lookup(k))
   */
  def alter(f: Option[A] => Option[A])(k: K)(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => f(None) match {
        case None => empty
        case Some(x) => singleton(k, x)
      }
      case Bin(sx, kx, x, l, r) => o.order(k, kx) match {
        case LT => balance(kx, x, go(l), r)
        case GT => balance(kx, x, l, go(r))
        case EQ => f(Some(x)) match {
          case Some(xx) => Bin(sx, kx, xx, l, r)
          case None => glue(l, r)
        }
      }
    }
    go(this)
  }

//  {--------------------------------------------------------------------
//    [merge l r]: merges two trees.
//  --------------------------------------------------------------------}
//  merge :: Map k a -> Map k a -> Map k a
//  merge Tip r   = r
//  merge l Tip   = l
//  merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
//    | delta*sizeL <= sizeR = balance ky y (merge l ly) ry
//    | delta*sizeR <= sizeL = balance kx x lx (merge rx r)
//    | otherwise            = glue l r


  private[data] def merge(m1: BMap[K, A], m2: BMap[K, A]): BMap[K, A] = (m1, m2) match {
    case (Tip(), r) => r
    case (l, Tip()) => l
    case (l@ Bin(sizeL, kx, x, lx, rx), r@Bin(sizeR, ky, y, ly, ry)) => {
      if ((delta*sizeL) <= sizeR) balance(ky, y, merge(l, ly), ry)
      else if ((delta*sizeR) <= sizeR) balance(kx, x, lx, merge(rx, r))
      else glue(l, r)
    }
  }

  private[data] def glue(left: BMap[K, A], right: BMap[K, A]): BMap[K, A] = (left, right) match {
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

  def deleteFindMin: ((K, A), BMap[K, A]) = deleteFindMinWith(this)

  private def deleteFindMinWith(m: BMap[K, A]): ((K, A), BMap[K, A]) = m match {
    case Bin(_, k, x, Tip(), r) => ((k, x), r)
    case Bin(_, k, x, l, r) => {
      val (km, ll) = deleteFindMinWith(l)
      (km, balance(k, x, ll, r))
    }
    case Tip() => sys.error("Map.deleteFindMin: can not return the minimal element of an empty map Tip")
  }

  def deleteFindMax: ((K, A), BMap[K, A]) = deleteFindMaxWith(this)

  private def deleteFindMaxWith(m: BMap[K, A]): ((K, A), BMap[K, A]) = m match {
    case Bin(_, k, x, l, Tip()) => ((k, x), l)
    case Bin(_, k, x, l, r) => {
      val (km, rr) = deleteFindMaxWith(r)
      (km, balance(k, x, l, rr))
    }
    case Tip() => sys.error("Map.deleteFindMax: can not return the minimal element of an empty map Tip")
  }

  def updateMin(f: A => Option[A]): BMap[K, A] = {
    updateMinWithKey(_ => f)
  }

  def updateMinWithKey(f: K => A => Option[A]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => empty
      case Bin(sx, kx, x, Tip(), r) => f(kx)(x) match {
        case None => r
        case Some(xl) => Bin(sx, kx, xl, empty, r)
      }
      case Bin(_, kx, x, l, r) => balance(kx, x, go(l), r)
    }
    go(this)
  }

  def updateMax(f: A => Option[A]): BMap[K, A] = {
    updateMaxWithKey(_ => f)
  }

  def updateMaxWithKey(f: K => A => Option[A]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => empty
      case Bin(sx, kx, x, l, Tip()) => f(kx)(x) match {
        case None => l
        case Some(xl) => Bin(sx, kx, xl, l, empty)
      }
      case Bin(_, kx, x, l, r) => balance(kx, x, l, go(r))
    }
    go(this)
  }

  def minViewWithKey: Option[((K, A), BMap[K, A])] = this match {
    case Tip() => None
    case _ => Some(deleteFindMin)
  }

  def maxViewWithKey: Option[((K, A), BMap[K, A])] = this match {
    case Tip() => None
    case _ => Some(deleteFindMax)
  }

  def minView: Option[(A, BMap[K, A])] = this match {
    case Tip() => None
    case _ => {
      val ((kx, x), m) = deleteFindMin
      Some(x, m)
    }
  }

  def maxView: Option[(A, BMap[K, A])] = this match {
    case Tip() => None
    case _ => {
      val ((kx, x), m) = deleteFindMax
      Some(x, m)
    }
  }

  def unions[B](ts: List[BMap[K, B]])(implicit o: Order[K]): BMap[K, B] = {
    ts.foldLeft(empty[K, B])((a, b) => b.union(a))
  }


  /**
   * O(n+m).
   * The expression t1.union(t2) takes the left-biased union of t1 and t2, preferring t1 when duplicate keys are encountered.
   * The implementation uses the efficient hedge-union algorithm.
   * Hedge-union is more efficient on (bigset \``union`\` smallset).
   */
  def union(m: BMap[K, A])(implicit o: Order[K]): BMap[K, A] = (this, m) match {
    case (Tip(), t2) => t2
    case (t1, Tip()) => t1
    case (t1, t2) => hedgeUnionL(_ => LT, _ => GT, t1, t2)
  }

  def hedgeUnionL(k1: K => Ordering, k2: K => Ordering, m1: BMap[K, A], m2: BMap[K, A])(implicit o: Order[K]): BMap[K, A] = (k1, k2, m1, m2) match {
    case (_, _, t1, Tip()) => t1
    case (cmplo, cmphi, Tip(), Bin(_, kx, x, l, r)) => join(kx, x, filterGt(cmplo, l), filterLt(cmphi, r))
    case (cmplo, cmphi, Bin(_, kx, x, l, r), t2) => {
      val cmpkx: K => Ordering = k => o.order(kx, k)
      join(kx, x, hedgeUnionL(cmplo, cmpkx, l, trim(cmplo, cmpkx, t2)), hedgeUnionL(cmpkx, cmphi, r, trim(cmpkx, cmphi, t2)))
    }
  }

  def hedgeUnionWithKey(k1: K => A => A => A)(k2: K => Ordering)(k3: K => Ordering)(m1: BMap[K, A])(m2: BMap[K, A])(implicit o: Order[K]): BMap[K, A] =
    (k1, k2, k3, m1, m2) match {
      case (_, _, _, t1, Tip()) => t1
      case (_, cmplo, cmphi, Tip(), Bin(_, kx, x, l, r)) => join(kx, x, (filterGt(cmplo, l)), filterLt(cmphi, r))
      case (f, cmplo, cmphi, Bin(_, kx, x, l, r), t2) => {
        val cmpkx = o.order(kx, _: K)
        val lt = trim(cmplo, cmpkx, t2)
        val (found, gt) = trimLookupLo(kx, cmphi, t2)
        val newx = found match {
          case Some(xy) => f(kx)(x)(xy._2)
          case None => x
        }
        join(kx, newx, hedgeUnionWithKey(f)(cmplo)(cmpkx)(l)(lt), hedgeUnionWithKey(f)(cmpkx)(cmphi)(r)(gt))
      }
    }

  def difference[B](m: BMap[K, B])(implicit o: Order[K]): BMap[K, A] = (this, m) match {
    case (Tip(), _) => empty[K, A]
    case (t1, Tip()) => t1
    case (t1, t2) => hedgeDiff(_ => Ordering.LT, _ => Ordering.GT, t1, t2)
  }

  private def hedgeDiff[C](k1: K => Ordering, k2: K => Ordering, m1: BMap[K, A], m2: BMap[K, C])(implicit o: Order[K]): BMap[K, A] = (k1, k2, m1, m2) match {
    case (_, _, Tip(), _) => empty[K, A]
    case (cmplo, cmphi, Bin(_, kx, x, l, r), Tip()) => join(kx, x, filterGt(cmplo, l), filterLt(cmphi, r))
    case (cmplo, cmphi, t, Bin(_, kx, _, l, r)) => {
      val cmpkx = o.order(kx, _: K)
      merge(hedgeDiff(cmplo, cmpkx, trim(cmplo, cmpkx, t), l), hedgeDiff(cmpkx, cmphi, trim(cmpkx, cmphi, t), r))
    }
  }

  def trim(k1: K => Ordering, k2: K => Ordering, m: BMap[K, A]): BMap[K, A] = (k1, k2, m) match {
    case (_, _, Tip()) => empty
    case (cmplo, cmphi, t@Bin(_, kx, _, l, r)) => cmplo(kx) match {
      case LT => cmphi(kx) match {
        case GT => t
        case _ => trim(cmplo, cmphi, l)
      }
      case _ => trim(cmplo, cmphi, r)
    }
  }

  def trimLookupLo(k1: K, k2: K => Ordering, m: BMap[K, A])(implicit o: Order[K]): (Option[(K, A)], BMap[K, A]) = (k1, k2, m) match {
    case (_, _, Tip()) => (None, empty)
    case (lo, cmphi, t@Bin(_, kx, x, l, r)) => o.order(lo, kx) match {
      case LT => cmphi(kx) match {
        case GT => (t.lookupAssoc(lo), t)
        case _ => trimLookupLo(lo, cmphi, l)
      }
      case GT => trimLookupLo(lo, cmphi, r)
      case EQ => (Some(kx, x), trim(o.order(lo, _: K), cmphi, r))
    }
  }

  def join(kx: K, x: A, m1: BMap[K, A], m2: BMap[K, A])(implicit o: Order[K]): BMap[K, A] = (m1, m2) match {
    case (Tip(), r) => insertMin(kx, x, r)
    case (l, Tip()) => insertMax(kx, x, l)
    case (l@Bin(sizeL, ky, y, ly, ry), r@Bin(sizeR, kz, z, lz, rz)) => {
      if (delta * sizeL <= sizeR) balance(kz, z, join(kx, x, l, lz), rz)
      else if (delta * sizeR <= sizeL) balance(ky, y, ly, join(kx, x, ry, r))
      else bin(kx, x, l, r)
    }
  }

  def insertMin(kx: K, x: A, m: BMap[K, A]): BMap[K, A] = m match {
    case Tip() => singleton(kx, x)
    case Bin(_, ky, y, l, r) => balance(ky, y, (insertMin(kx, x, l)), r)
  }

  def insertMax(kx: K, x: A, m: BMap[K, A]): BMap[K, A] = m match {
    case Tip() => singleton(kx, x)
    case Bin(_, ky, y, l, r) => balance(ky, y, l, (insertMax(kx, x, r)))
  }

  def filterGt(cmp: K => Ordering, m: BMap[K, A])(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => empty
      case Bin(_, kx, x, l, r) => cmp(kx) match {
        case LT => join(kx, x, go(l), r)
        case GT => go(r)
        case EQ => r
      }
    }
    go(m)
  }

  def filterLt(cmp: K => Ordering, m: BMap[K, A])(implicit o: Order[K]): BMap[K, A] = {
    def go(m: BMap[K, A]): BMap[K, A] = m match {
      case Tip() => empty
      case Bin(_, kx, x, l, r) => cmp(kx) match {
        case LT => go(l) join(kx, x, go(l), r)
        case GT => join(kx, x, l, go(r))
        case EQ => l
      }
    }
    go(m)
  }

  def map[B](f: A => B): BMap[K, B] = mapWithKey(_ => f)

  def mapWithKey[B](f: K => A => B): BMap[K, B] = {
    def go(m: BMap[K, A]): BMap[K, B] = m match {
      case Tip() => empty
      case Bin(sx, kx, x, l, r) => Bin(sx, kx, f(kx)(x), go(l), go(r))
    }
    go(this)
  }

  //TODO does it actually makes sense to define flatMap like this? the function needs to construct a map from a single value with keys it can't re-use.
  //perhaps the default flatMap should be what is now defined as flatMapWithKey. This would impact the default map implementation as well, however, to be consistent.
  def flatMap[B](f: A => BMap[K, B])(implicit o: Order[K]): BMap[K, B] = flatMapWithKey(_ => f)

  def flatMapWithKey[B](f: K => A => BMap[K, B])(implicit o: Order[K]): BMap[K, B] = {
    def go(m: BMap[K, A]): BMap[K, B] = m match {
      case Tip() => empty
      case Bin(sx, kx, x, l, r) => {
        val maps = List(f(kx)(x), go(l), go(r))
        unions(maps)
      }
    }
    go(this)
  }

  def foldr[B](f: (A) => (=> B) => B)(b: B): B = foldrWithKey(_ => f)(b)

  def foldrWithKey[B](f: K => (A) => (=> B) => B)(b: B): B = {
    def go(z: B, m: BMap[K, A]): B = (z, m) match {
      case (z, Tip()) => z
      case (z, Bin(_, kx, x, l, r)) => go(f(kx)(x)(go(z, r)), l)
    }
    go(b, this)
  }

  def foldl[B](f: B => (=> A) => B)(b: B): B = foldlWithKey(_ => f)(b)

  /**
   * O(n). Pre-order fold. The function will be applied from the highest value to the lowest. 
   */
  def foldlWithKey[B](f: K => B => (=> A) => B)(b: B): B = {
    def go(z: B, m: BMap[K, A]): B = (z, m) match {
      case (z, Tip()) => z
      case (z, Bin(_, kx, x, l, r)) => go(f(kx)(go(z, l))(x), r)
    }
    go(b, this)
  }

  def toStream: Stream[(K, A)] = foldrWithKey[Stream[(K, A)]](k => x => xs => Stream.cons((k, x), xs))(Stream())

  def toUnsortedList: List[(K, A)] = toStream.toList

  def toList: List[(K, A)] = toAscList

  //  import wrap.StreamW._
  //  def toStream: String[(K, V)] = foldrWithKey(Stream[(K, V)])(k => x => xs => (k,x))

  def toAscList: List[(K, A)] = foldrWithKey[List[(K, A)]](k => x => xs => (k, x) :: xs)(Nil)

  def toDescList: List[(K, A)] = foldlWithKey[List[(K, A)]](k => xs => x => (k, x) :: xs)(Nil)

  //debugging /- printing the map
  def showTree: String = showTreeWith(k => v => k.toString ++ ":=" ++ v.toString, true, true)

  def showTreeWith(showelem: K => A => String, hang: Boolean, wide: Boolean): String = {
    if (hang) showsTreeHang(showelem, wide, List(), this)
    else showsTreeHang(showelem, wide, List(), this) //TODO obviously...
  }

  def showsTree(showelem: K => A => String, wide: Boolean, lbars: List[String], rbars: List[String], t: BMap[K, A]): String = t match {
    case Tip() => showsBars(lbars)
    case _ => ""
  }

  def showsTreeHang(showelem: K => A => String, wide: Boolean, bars: List[String], t: BMap[K, A]): String = t match {
    case Tip() => showsBars(bars) ++ "|\n"
    case Bin(_, kx, x, Tip(), Tip()) => showsBars(bars) ++ showelem(kx)(x) ++ "\n"
    case Bin(_, kx, x, l, r) => {
      //TODO there must be a better way to do this...
      val sb = showsBars(bars) 
      val se = showelem(kx)(x)
      val newLine = "\n"
      val sw = showWide(wide, bars)
      val sth = showsTreeHang(showelem, wide, withBar(bars), l)
      val sthe = showsTreeHang(showelem, wide, (withEmpty(bars)), r)

      sb ++ se ++ newLine + sw ++ sth ++ sw ++ sthe
//      showsBars(bars) ++ showelem(kx)(x) ++ "\n" ++ showWide(wide, bars) ++ showsTreeHang(showelem, wide, withBar(bars), l) ++ showWide(wide, bars) ++ showsTreeHang(showelem, wide, (withEmpty(bars)), r)
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


  private def balance(k: K, v: A, l: BMap[K, A], r: BMap[K, A]): BMap[K, A] = {
    val sizeL = l.size
    val sizeR = r.size
    val sizeX = sizeL + sizeR + 1
    if (sizeL + sizeR <= 1) Bin(sizeX, k, v, l, r)
    else if (sizeR >= delta * sizeL) rotateL(k, v, l, r)
    else if (sizeL >= delta * sizeR) rotateR(k, v, l, r)
    else Bin(sizeX, k, v, l, r)
  }

  private def rotateL(k: K, v: A, l: BMap[K, A], r: BMap[K, A]): BMap[K, A] = r match {
    case Bin(_, _, _, ly, ry) if (ly.size < ratio * ry.size) => singleL(k, v, l, r)
    case Bin(_, _, _, ly, ry) => doubleL(k, v, l, r)
    case Tip() => sys.error("rotateL tip")
  }

  private def singleL(k1: K, v1: A, l1: BMap[K, A], r2: BMap[K, A]): BMap[K, A] = r2 match {
    case Bin(_, k2, v2, l2, r2) => bin(k2, v2, bin(k1, v1, l1, l2), r2)
    case Tip() => sys.error("singleL Tip")
  }

  private def doubleL(k1: K, x1: A, t1: BMap[K, A], b: BMap[K, A]): BMap[K, A] = b match {
    case Bin(_, k2, x2, Bin(_, k3, x3, t2, t3), t4) => bin(k3, x3, bin(k1, x1, t1, t2), bin(k2, x2, t3, t4))
    case _ => sys.error("doubleL")
  }

  private def rotateR(k: K, v: A, l: BMap[K, A], r: BMap[K, A]): BMap[K, A] = l match {
    case Bin(_, _, _, ly, ry) if (ry.size < ratio * ly.size) => singleR(k, v, l, r)
    case Bin(_, _, _, ly, ry) => doubleR(k, v, l, r)
    case Tip() => sys.error("rotateL tip")
  }

  private def singleR(k1: K, x1: A, b: BMap[K, A], t3: BMap[K, A]): BMap[K, A] = b match {
    case Bin(_, k2, x2, t1, t2) => bin(k2, x2, t1, bin(k1, x1, t2, t3))
    case Tip() => sys.error("singleR Tip")
  }

  private def doubleR(k1: K, x1: A, b: BMap[K, A], t4: BMap[K, A]): BMap[K, A] = b match {
    case Bin(_, k2, x2, t1, Bin(_, k3, x3, t2, t3)) => bin(k3, x3, bin(k2, x2, t1, t2), bin(k1, x1, t3, t4))
    case _ => sys.error("doubleR")
  }

  override def toString = "BMap (" + toStream.take(10).force + "...)"
  
  //some utility functions
  def ===(m: BMap[K, A])(implicit e: Equal[BMap[K, A]]): Boolean = e.equal(this, m)
}

trait BMaps {

  import BMap._

  implicit def bmap[K: Order, V] = new Applicative[({type l[a]=BMap[K, a]})#l] with Traverse[({type l[a]=BMap[K, a]})#l] with Equal[BMap[K, V]] {
    def ap[A, B](fa: ({type l[a] = BMap[K, a]})#l[A])(f: ({type l[a] = BMap[K, a]})#l[(A) => B]) = null

    def equal(a1: BMap[K, V], a2: BMap[K, V]) = (a1.size == a2.size) && (a1.toList == a2.toList)

    override def map[A, B](fa: BMap[K, A])(f: A => B): BMap[K, B] = fa map f

    def foldR[A, B](fa: BMap[K, A], z: B)(f: (A) => (=> B) => B): B = fa.foldr(f)(z)
    
    def pure[A](a: => A) = BMap.empty[K, A]

    def traverseImpl[F[_], A, B](m: BMap[K, A])(f: (A) => F[B])(implicit F: Applicative[F]): F[BMap[K, B]] = {
      def mkBin[K, V](s: Int)(k: K)(v: V)(l: BMap[K, V])(r: BMap[K, V]): BMap[K, V] = Bin(s, k, v, l, r)
      m match {
        case Tip() => F.pure(Tip[K, B])
        case Bin(s, k, v, l, r) => {
          val fvm = F.map(f(v))(a => mkBin(s)(k)(a)_)
          F.ap(traverseImpl(r)(f))(F.ap(traverseImpl[F, A, B](l)(f))(fvm))
        }
      }
    }
  }
}

object BMap extends BMaps {



  val delta = 4
  val ratio = 2

  def fromList[K, V](l: List[(K, V)])(implicit o: Order[K]): BMap[K, V] = l.foldLeft(empty[K, V])((t, kv) => t.insert(kv._1, kv._2))

  def singleton[K, V](k: K, v: V): BMap[K, V] = Bin(1, k, v, empty, empty)

  def empty[K, V]: BMap[K, V] = Tip[K, V]

  /**The empty map */
  object Tip {
    def apply[K, V]: BMap[K, V] = new BMap[K, V] {
      def fold[R](empty: => R, nonempty: (Int, K, V, BMap[K, V], BMap[K, V]) => R) = empty

      override def toString = "<tip>"
    }

    def unapply[K, V](m: BMap[K, V]): Boolean = m.fold(true, (_, _, _, _, _) => false)
  }

  def bin[K, V](k: K, v: V, l: BMap[K, V], r: BMap[K, V]): BMap[K, V] = Bin(l.size + r.size + 1, k, v, l, r)

  case class Bin[K, V](s: Int, k: K, v: V, left: BMap[K, V], right: BMap[K, V]) extends BMap[K, V] {
    def fold[R](empty: => R, nonempty: (Int, K, V, BMap[K, V], BMap[K, V]) => R) = nonempty(s, k, v, left, right)
  }
}

