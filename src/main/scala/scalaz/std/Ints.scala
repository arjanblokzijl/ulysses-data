package scalaz
package std

trait Ints {
  implicit object int extends Monoid[Int] with Show[Int] with Equal[Int] with Order[Int]{
    def append(f1: Int, f2: => Int): Int = f1 + f2
    def zero: Int = 0
    def show(f: Int): List[Char] = f.toString.toList

    def order(x: Int, y: Int): Ordering = if (x < y) Ordering.LT else if (x > y) Ordering.GT else Ordering.EQ
  }
}

object Int extends Ints