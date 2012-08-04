package data

import scalaz._
import scalaz.Ordering._
import scala.annotation._
/**
 * A port of haskell's Data.Set based on a size balanced tree.
 * @see http://haskell.org/ghc/docs/latest/html/libraries/containers-0.4.2.1/Data-Set.html
 * Contrary to haskell's version, this Set is strict, and not lazy.
 */
sealed trait BSet[A] {

  import BSet._

  type ShowS = String => String

  def showString(s: String): String = s

  def fold[R](empty: R, nonempty: (Int, A, BSet[A], BSet[A]) => R): R

  def toList: List[A] = fold(Nil, (i, a, l, r) => a :: (DList.fromList(l.toList) ++ DList.fromList(r.toList)).toList)

  def toAscList: List[A] = foldRight(List[A]())(_ :: _)

  def toSet: Set[A] = toList.toSet

  def toStream: Stream[A] = fold(Stream.empty[A], (i, a, l, r) => a #:: l.toStream #::: r.toStream)

  def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F], O1: Order[A], O2: Order[B]): F[BSet[B]] =
    DList.fromList(toList).foldr(F.point(BSet.empty[B])) {
      (a, fbs) => F.map2(f(a), fbs)((a, b) => b insert a)
    }

  def map[B](f: A => B)(implicit O1: Order[A], O: Order[B]): BSet[B] = fromList(DList.fromList(toList).map(f).toList)

  def foreach[B](f: A => B): Unit = this match {
    case Tip() => ()
    case Bin(_, a, l, r) => {
      l.foreach(f)
      f(a)
      r.foreach(f)
    }
  }

  def flatMap[B](f: A => BSet[B])(implicit O: Order[B]): BSet[B] = {
    def go(s: BSet[A]): BSet[B] =
      s.fold[BSet[B]](empty[B], (i, a, l, r) =>
        f(a).union(go(l)).union(go(r))
      )

    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    //avoid blowing the stack
    import scala.collection.mutable.ArrayStack
    val s = new ArrayStack[A]
    def go(bs: BSet[A]): Unit = bs match {
      case Tip() => ()
      case Bin(_, a, l, r) => {
        go(l)
        s += a
        go(r)
      }
    }
    go(this)
    var r = z
    while (!s.isEmpty) {
      // force and copy the value of r to ensure correctness
      val w = r
      r = f(s.pop, w)
    }
    r
  }


  /**
   * O(log n). Inserts an element in the set. If set already contains
   * an element equal to the given value, it is replaced with the new value.
   */
  def insert(a: A)(implicit O: Order[A]): BSet[A] = {
    def go(x: A, s: => BSet[A]): BSet[A] = s.fold(singleton(x),
      (sz, y, l, r) => O.order(x, y) match {
        case LT => balanceL(y, go(x, l), r)
        case GT => balanceR(y, l, go(x, r))
        case EQ => Bin(sz, a, l, r)
      }
    )
    go(a, this)
  }

  /**
   * O(log n). Inserts an element in the set. If set already contains
   * an element equal to the given value, it is replaced with the new value.
   */
  def insertR(a: A)(implicit O: Order[A]): BSet[A] = {
    def go(x: A, s: => BSet[A]): BSet[A] = s match {
      case Tip() => singleton(a)
      case Bin(b, y, l, r) => O.order(x, y) match {
        case LT => balanceL(y, go(x, l), r)
        case GT => balanceR(y, l, go(x, r))
        case EQ => Bin(b, a, l, r)
      }
    }
    go(a, this)
  }


  /**O(log n). Deletes an element from the set.
   * If the element is not in the set, the existing set is returned.
   * */
  def delete(a: A)(implicit O: Order[A]): BSet[A] = {
    def go(a: A, s: BSet[A]): BSet[A] = (a, s) match {
      case (_, Tip()) => empty
      case (x, Bin(_, y, l, r)) => O.order(x, y) match {
        case LT => balanceR(y, go(x, l), r)
        case GT => balanceL(y, l, go(x, r))
        case EQ => glue(l, r)
      }
    }
    go(a, this)
  }

  /**
   * O(1). Is the set empty?
   * empty.isEmpty => true
   * singleton(""a").isEmpty => false
   */
  def isEmpty: Boolean = size == 0

  /**
   * O(1) the number of elements in the set
   */
  def size: Int = fold(0, (s, _, _, _) => s)

  /**
   * Alias for `size`.
   */
  def length: Int = size

  def member(a: A)(implicit O: Order[A]): Boolean = {
    def go(x: A, s: BSet[A]): Boolean = fold(false, (_, y, l, r) => O.order(x, y) match {
      case EQ => true
      case LT => go(x, r)
      case GT => go(x, l)
    })
    go(a, this)
  }

  def notMember(a: A)(implicit O: Order[A]): Boolean = !member(a)

  def union(that: BSet[A])(implicit O: Order[A]): BSet[A] = (this, that) match {
    case (Tip(), s2) => s2
    case (s1, Tip()) => s1
    case (s1, s2) => hedgeUnion(None, None, s1, s2)
  }

  private def hedgeUnion(blo: Option[A], bhi: Option[A], s1: BSet[A], s2: BSet[A])(implicit O: Order[A]): BSet[A] =
    (s1, s2) match {
      case (t1, Tip()) => t1
      case (Tip(), Bin(_, x, l, r)) => join(x, filterGt(blo, l), filterLt(bhi, r))
      case (t1, Bin(_, x, Tip(), Tip())) => t1.insertR(x)
      case (Bin(_, x, l, r), t2) => join(x, hedgeUnion(blo, Some(x), l, trim(blo, Some(x), t2)), hedgeUnion(Some(x), bhi, r, trim(Some(x), bhi, t2)))
    }

  private def filterGt(a: Option[A], s: BSet[A])(implicit O: Order[A]): BSet[A] = {
    def filter1(b1: A, s1: BSet[A]): BSet[A] =
      s1.fold(empty, (_, x, l, r) => O.order(b1, x) match {
        case LT => join(x, filter1(b1, l), r)
        case EQ => r
        case GT => filter1(b1, r)
      })
    a match {
      case None => s
      case Some(b) => filter1(b, s)
    }
  }

  def filterLt(a: Option[A], s: BSet[A])(implicit O: Order[A]): BSet[A] = {
    def filter1(b1: A, s1: BSet[A]): BSet[A] =
      s1.fold(empty, (_, x, l, r) => O.order(x, b1) match {
        case LT => join(x, l, filter1(b1, r))
        case EQ => l
        case GT => filter1(b1, l)
      })
    a match {
      case None => s
      case Some(b) => filter1(b, s)
    }
  }

  private def trim(x1: Option[A], x2: Option[A], s: BSet[A])(implicit O: Order[A]): BSet[A] = {
    def greater(lo: A, s: BSet[A]): BSet[A] =
      s.fold(s, (_, x, _, r) => if (O.lessThanOrEqual(x, lo)) greater(lo, r) else s)
    def lesser(hi: A, s: BSet[A]): BSet[A] =
      s.fold(s, (_, x, l, r) => if (O.greaterThanOrEqual(x, hi)) lesser(hi, l) else s)
    def middle(lo: A, hi: A, s: BSet[A]): BSet[A] =
      s.fold(s, (_, x, l, r) =>
        if (O.lessThanOrEqual(x, lo)) middle(lo, hi, r)
        else if (O.greaterThanOrEqual(x, hi)) middle(lo, hi, l)
        else s
      )

    (x1, x2) match {
      case (None, None) => s
      case (Some(lx), None) => greater(lx, s)
      case (None, Some(hx)) => lesser(hx, s)
      case (Some(lx), Some(hx)) => middle(lx, hx, s)
    }
  }

  private def join(x: A, s1: BSet[A], s2: BSet[A])(implicit O: Order[A]): BSet[A] = (s1, s2) match {
    case (Tip(), r) => insertMin(x, r)
    case (l, Tip()) => insertMax(x, l)
    case (l@Bin(sizeL, y, ly, ry), r@Bin(sizeR, z, lz, rz)) =>
      if (delta * sizeL <= sizeR) balanceL(z, join(x, l, lz), rz)
      else if (delta * sizeR <= sizeL) balanceR(y, ly, join(x, ry, r))
      else bin(x, l, r)
  }

  private def insertMin(x: A, s: BSet[A]): BSet[A] =
    s.fold(singleton(x),
      (_, y, l, r) => balanceL(y, (insertMin(x, l)), r)
    )

  private def insertMax(x: A, s: BSet[A]): BSet[A] =
    s.fold(singleton(x),
      (_, y, l, r) => balanceR(y, l, (insertMax(x, r)))
    )

  /**
   * glues two trees together, assuming that these are a already balanced with respect to each other.
   */
  private def glue(ls: BSet[A], rs: BSet[A]): BSet[A] = (ls, rs) match {
    case (Tip(), r) => r
    case (l, Tip()) => l
    case (l, r) =>
      if (l.size > r.size) {
        val (m, l1) = l.deleteFindMax
        balanceR(m, l1, r)
      } else {
        val (m,r1) = r.deleteFindMin
        balanceL(m, l, r1)
      }
  }

  def findMin: A = this match {
    case Tip() => sys.error("findMin: the empty set has no elements")
    case Bin(_, x, Tip(), _) => x
    case Bin(_, _, l, _) => l.findMin
  }

  def findMax: A = this match {
    case Tip() => sys.error("findMin: the empty set has no elements")
    case Bin(_, x, _, Tip()) => x
    case Bin(_, _, _, r) => r.findMax
  }

  def deleteFindMin: (A, BSet[A]) = this match {
    case Tip() => sys.error("deleteFindMin: cannot delete the minimal element of an empty set")
    case Bin(_, x, Tip(), r) => (x, r)
    case Bin(_, x, l, r) => {
      val (xm, l1) = l.deleteFindMin
      (xm, balanceR(x, l1, r))
    }
  }

  def deleteFindMax: (A, BSet[A]) = this match {
    case Tip() => sys.error("deleteFindMax: cannot delete the maximum element of an empty set")
    case Bin(_, x, l, Tip()) => (x, l)
    case Bin(_, x, l, r) => {
      val (xm, r1) = r.deleteFindMax
      (xm, balanceL(x, l, r1))
    }
  }

  //debugging /- printing the set
  def showTree: String = showTreeWith(true, true)

  def showTreeWith(hang: Boolean, wide: Boolean): String = {
    if (hang) showsTreeHang(wide, List(), this)
    else showsTreeHang(wide, List(), this) //TODO obviously...
  }

  def showsTree(showelem: A => String, wide: Boolean, lbars: List[String], rbars: List[String], t: BSet[A]): String = t match {
    case Tip() => showsBars(lbars)
    case _ => ""
  }

  def showsTreeHang(wide: Boolean, bars: List[String], t: BSet[A]): String = t match {
    case Tip() => showsBars(bars) ++ "|\n"
    case Bin(_, x, Tip(), Tip()) => showsBars(bars) ++ x.toString ++ "\n"
    case Bin(_, x, l, r) => {
      //TODO there must be a better way to do this...
      val sb = showsBars(bars) + x.toString + "\n"
      val sw = showWide(wide, bars)
      val sth = showsTreeHang(wide, withBar(bars), l)
      val sthe = showsTreeHang(wide, (withEmpty(bars)), r)
      sb + sw + sw + sth + sw + sthe
    }
  }

  private val node: String = "+--"

  private def showWide(wide: Boolean, bars: List[String]): String =
    if (wide) bars.reverse.mkString + "|\n"
    else bars.mkString


  private def showsBars(bars: List[String]): String = bars match {
    case List() => ""
    case _ => bars.tail.reverse.mkString + node
  }

  private def withBar(bars: List[String]): List[String] =  "|  " :: bars

  private def withEmpty(bars: List[String]): List[String] = "  " :: bars

  def ===(m: BSet[A])(implicit e: Equal[BSet[A]]): Boolean = e.equal(this, m)

  private[data] def balanced: Boolean = this match {
    case Tip() => true
    case Bin(_, _, l, r) => ((l.size + r.size <= 1) || (l.size <= delta * r.size) && (r.size <= delta * l.size)) && l.balanced && r.balanced
  }
}

trait BSetInstances {

  import BSet._

  import std.list._
  implicit def bsetEqual[A: Equal] = new Equal[BSet[A]] {
    def equal(s1: BSet[A], s2: BSet[A]) = s1.size == s2.size && Equal[List[A]].equal(s1.toList, s2.toList)
  }

  implicit def bsetOrder[A: Order] = new Order[BSet[A]] {
    def order(s1: BSet[A], s2: BSet[A]) = Order[List[A]].order(s1.toList, s2.toList)
  }

  implicit def bsetMonoid[A: Order] = new Monoid[BSet[A]] {
    def append(f1: BSet[A], f2: => BSet[A]): BSet[A] = f1 union f2
    def zero: BSet[A] = empty[A]
  }
}

object BSet extends BSetInstances {
  val delta = 3
  val ratio = 2

  def fromList[A](l: List[A])(implicit O: Order[A]): BSet[A] = {
    @tailrec
    def go(as: List[A], acc: BSet[A]): BSet[A] = as match {
      case Nil => acc
      case x :: xs => go(xs, acc.insert(x))
    }
    go(l, empty)
  }

  def fromAscList[A](l: List[A]): BSet[A] = fromDistinctAscList(l.distinct)
  /**
   * O(n). Build a set from an ascending list of distinct elements in linear time.
   * The precondition (the list strictly ascending) is not checked.
   */
  def fromDistinctAscList[A](l: List[A]): BSet[A] = {
    def create(c: BSet[A] => List[A] => BSet[A], n: Int, as: List[A]): BSet[A] = {
      (n, as) match {
        case (0, xs) => c(empty[A])(xs)
        case (5, xs) => xs match {
          case (x1 :: x2 :: x3 :: x4 :: x5 :: xx) => c(bin(x4, bin(x2, singleton(x1), singleton(x3)), singleton(x5)))(xs)
          case _ => sys.error("fromdistinctAscList create 5")
        }
        case (n, xs) => {
          val nl = n / 2
          val nr = n - nl - 1
          create(createR(nr, c), nl, xs)
        }
      }
    }
    def createR(n: Int, c: BSet[A] => List[A] => BSet[A])(l: BSet[A])(as: List[A]) = as match {
      case x :: ys => create(createB(l, x, c), n, ys)
      case Nil => sys.error("fromDistinctAscList createR Nil")
    }
    def createB(l: BSet[A], x: A, c: BSet[A] => List[A] => BSet[A])(r: BSet[A])(zs: List[A]) =
      c(bin(x, l, r))(zs)

    create(Function.const[BSet[A], List[A]] _, l.length, l)
  }

  def singleton[A](a: => A): BSet[A] = bin(a, empty, empty)

  def empty[A]: BSet[A] = Tip[A]


  /**The empty set */
  object Tip {
    def apply[A]: BSet[A] = new BSet[A] {
      def fold[R](empty: R, nonempty: (Int, A, BSet[A], BSet[A]) => R) = empty
      override def toString = "<tip>"
    }

    def unapply[A](m: BSet[A]): Boolean = m.fold(true, (_, _, _, _) => false)
  }

  /**The non-empty set */
  object Bin {
    def apply[A](i: Int, a: A, l: BSet[A], r: => BSet[A]): BSet[A] = new BSet[A] {
      def fold[R](empty: R, nonempty: (Int, A, BSet[A], BSet[A]) => R) = nonempty(i, a, l, r)

      override def toString = "<bin>"
    }

    def unapply[A](s: BSet[A]): Option[(Int, A, BSet[A], BSet[A])] = s.fold(None, (s, x, l, r) => Some((s, x, l, r)))
  }

  def bin[A](a: A, l: BSet[A], r: BSet[A]): BSet[A] = Bin(l.size + r.size + 1, a, l, r)

  def balanceL[A](x: A, l: BSet[A], r: BSet[A]): BSet[A] = r match {
    case Tip() => l match {
      case Tip() => Bin(1, x, empty, empty)
      case Bin(_, _, Tip(), Tip()) => Bin(2, x, l, empty)
      case Bin(_, lx, Tip(), Bin(_, lrx, _, _)) => Bin(3, lrx, Bin(1, lx, empty, empty), Bin(1, x, empty, empty))
      case Bin(_, lx, ll@Bin(_, _, _, _), Tip()) => Bin(3, lx, ll, Bin(1, x, empty, empty))
      case Bin(ls, lx, ll@Bin(lls, _, _, _), lr@Bin(lrs, lrx, lrl, lrr)) =>
        if (lrs < ratio * lls) Bin(1+ls, lx, ll, Bin(1+lrs, x, lr, empty))
        else Bin(1+ls, lrx, Bin(1 + lls + lrl.size, lx, ll, lrl), Bin(1+lrr.size, x, lrr, empty))
    }
    case Bin(rs, _, _, _) => l match {
      case Tip() => Bin(1+rs, x, empty, r)
      case Bin(ls, lx, ll, lr) =>
      if (ls > delta*rs) (ll, lr) match {
        case (Bin(lls, _, _, _), Bin(lrs, lrx, lrl, lrr)) =>
          if (lrs < ratio*lls) Bin(1+ls+rs, lx, ll, Bin(1+rs+lrs, x, lr, r))
          else Bin(1+ls+rs, lrx, Bin(1+lls+lrl.size, lx, ll, lrl), Bin(1+rs+lrr.size, x, lrr, r))
        case (_, _) => sys.error("Failure in Data.Set.balanceL")
      }
      else Bin(1+ls+rs, x, l, r)
    }
  }

  def balanceR[A](x: A, l: BSet[A], r: BSet[A]): BSet[A] = l match {
    case Tip() => r match {
      case Tip() => Bin(1, x, empty, empty)
      case Bin(_, _, Tip(), Tip()) => Bin(2, x, empty, r)
      case Bin(_, rx, Tip(), rr@Bin(_, _, _, _)) => Bin(3, rx, Bin(1, x, empty, empty), rr)
      case Bin(_, rx, Bin(_, rlx, _, _), Tip()) => Bin(3, rlx, Bin(1, x, empty, empty), Bin(1, rx, empty, empty))
      case Bin(rs, rx, rl@Bin(rls, rlx, rll, rlr), rr@Bin(rrs, _, _, _)) =>
        if (rls < ratio*rrs) Bin(1+rs, rx, Bin(1+rls, x, empty, rl), rr)
        else Bin(1+rs, rlx, Bin(1+rll.size, x, empty, rll), Bin(1+rrs+rlr.size, rx, rlr, rr))
    }
    case Bin(ls, _, _, _) => r match {
      case Tip() => Bin(1+ls, x, l, empty)
      case Bin(rs, rx, rl, rr) =>
        if (rs > delta*ls) (rl, rr) match {
          case (Bin(rls, rlx, rll, rlr), Bin(rrs, _, _, _)) =>
            if (rls < ratio*rrs) Bin(1+ls+rs, rx, Bin(1+ls+rls, x, l, rl), rr)
            else Bin(1+ls+rs, rlx, Bin(1+ls+rll.size, x, l, rll), Bin(1+rrs+rlr.size, rx, rlr, rr))
          case (_, _) => sys.error("Failure in Data.BSet.balanceR")
          } else Bin(1+ls+rs, x, l, r)
      }
    }

}
