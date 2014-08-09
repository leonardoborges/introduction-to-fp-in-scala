package intro

/**
 * A monoid is an identity element paired with an associative
 * binary operation such that the following laws hold:
 *
 * associative:
 *  op(a, op(b, c)) == op(op(a, b), c)
 *
 * right identity:
 *  op(a, identity) == a
 *
 * left identity:
 *  op(identity, a) == a
 */
trait Monoid[A] {
  def identity: A
  def op(x: A, y: A): A
}

/* Useful datatypes that have a Monoid */
case class Sum(n: Int)
case class Product(n: Int)
case class Min(n: Int)
case class Max(n: Int)
case class Size(n: Int)
case class Endo[A](f: A => A)
case class First[A](first: Option[A]) // Errata: should it be Option[A]
case class Last[A](last: Option[A])

object Monoid {
  /**
   * Convenience for summoning a Monoid instance.
   *
   * usage: Monoid[String].op("yo", "lo")
   */
  def apply[A: Monoid]: Monoid[A] =
    implicitly[Monoid[A]]

  /* Monoid instances */

  /** Exercise 1: A monoid which takes the sum of the underlying integer values */
  implicit def SumMonoid: Monoid[Sum] = new Monoid[Sum]{
    def identity: Sum = Sum(0)
    def op(x: Sum, y: Sum): Sum = (x,y) match {
      case (Sum(n), Sum(m)) => Sum(m + n)
    }
  }


  /** Exercise 2: A monoid which takes the multiplication of the underlying integer values */
  implicit def ProductMonoid: Monoid[Product] = new Monoid[Product]{
    def identity: Product = Product(1)
    def op(x: Product, y: Product) = (x,y) match {
      case (Product(n), Product(m)) => Product(m * n)
    }
  }

  /** Exercise 3: A monoid which takes the minimum of the underlying integer values */
  implicit def MinMonoid: Monoid[Min] = new Monoid[Min]{
    def identity = Min(Integer.MAX_VALUE)
    def op(x: Min, y: Min) = (x,y) match {
      case (Min(n), Min(m)) => Min(List(n,m).min)
    }
  }

  /** Exercise 4: A monoid which takes the maximum of the underlying integer values */
  implicit def MaxMonoid: Monoid[Max] = new Monoid[Max]{
    def identity = Max(Integer.MIN_VALUE)
    def op(x: Max, y: Max) = (x,y) match {
      case (Max(n), Max(m)) => Max(List(n,m).max)
    }
  }

  /** Exercise 5: A monoid which counts the number of underlying values */
  implicit def SizeMonoid: Monoid[Size] = new Monoid[Size]{
    def identity = Size(0)
    def op(x: Size, y: Size) = (x,y) match {
      case (Size(n), Size(m)) => Size(m + n)
    }
  }

  /** Exercise 6: A monoid which composes the underlying functions */
  implicit def EndoMonoid[A]: Monoid[Endo[A]] = new Monoid[Endo[A]]{
    def identity = Endo(a => a)
    def op(x: Endo[A], y: Endo[A]) = (x,y) match {
      case (Endo(f), Endo(g)) => Endo(f compose g)
    }
  }


  /** Exercise 7: A monoid which always takes the first value */
  implicit def FirstMonoid[A]: Monoid[First[Option[A]]] = new Monoid[First[Option[A]]]{
    def identity = First(None)
    def op(x: First[Option[A]], y: First[Option[A]]) = (x,y) match {
      case (First(Some(_)), _) => x
      case _                   => y
    }
  }

  /** Exercise 8: A monoid which always takes the last value */
  implicit def LastMonoid[A]: Monoid[Last[Option[A]]] = new Monoid[Last[Option[A]]]{
    def identity = Last(None)
    def op(x: Last[Option[A]], y: Last[Option[A]]) = (x,y) match {
      case (_, Last(Some(_))) => y
      case _                  => x
    }
  }

  /** Exercise 9: A monoid which concatenates lists */
  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]{
    def identity = List()
    def op(x: List[A], y: List[A]) = x ++ y
  }


  /** Exercise 10: A monoid which unions the map, applying op to merge values */
  implicit def MapMonoid[A, B: Monoid]: Monoid[Map[A, B]] = new Monoid[Map[A, B]]{
    def identity: Map[A, B] = Map()
    def op(x: Map[A, B], y: Map[A, B]): Map[A, B] = x ++ y
  }


  /*  Monoid library */

  import MonoidSyntax._

  /**
   * Exercise 11
   *
   * Accumulate a value by summing the result of the map function `f`
   * applied to each element.
   *
   * scala> foldMap(List(1, 2, 3, 4, 5))(x => Sum(x))
   *  = Sum(15)
   */
  def foldMap[A, B: Monoid](xs: List[A])(f: A => B): B =
    xs.foldRight(Monoid[B].identity)((a, acc) => Monoid[B].op(f(a), acc))

  /**
   * Exercise 12
   *
   * Sum all the elements in the lest using the Monoid for A.
   *
   * scala> sum(List("hello", " ", "world"))
   *  = "hello world"
   */
  def sum[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].identity)(Monoid[A].op)
}

object MonoidSyntax {
  implicit class AnyMonoidSyntax[A: Monoid](value: A) {
    def |+|(other: A) =
      Monoid[A].op(value, other)
  }
}

/**
 * *Challenge* Exercise 13
 *
 * We have a data set of ticker prices. Produces a set of statistics for each
 * unique ticker name.
 */
object MonoidChallenge {
  import MonoidSyntax._
  import MonoidTupleInstances._

  case class Stats(min: Int, max: Int, total: Int, count: Int, average: Int)
  case class Stock(ticker: String, date: String, cents: Int)

  /**
   * Compute a map of ticker -> stats from the given data, ignoring any data
   * points that do _not_ match predicate.
   *
   * We want to do this with a single pass over the input data (doing a secondary
   * pass across the output is ok), so do not use list.filter(..).
   *
   * Example, filter out 0 values as they are bad data:
   *   MonoidChallenge.compute(MonoidChallenge.Data, stock => stock.cents != 0)
   *
   * Example, only include particular days of data:
   *   MonoidChallenge.compute(MonoidChallenge.Data, stock => stock.date == "2012-01-01" || stock.date == "2012-01-02")
   *
   * Note there are monoid instances for tuples whos components are all monoids
   * (this may be useful, but is not the only way to solve this problem).
   */
  def compute(data: List[Stock], predicate: Stock => Boolean): Map[String, Stats] =
    data.filter(predicate(_)).groupBy(_.ticker).map({
      case (ticker, stocks) =>
        val (Min(min), Max(max), Sum(sum), Size(size)) = Monoid.foldMap(stocks) {
          case Stock(_, data, cents) =>
            (Min(cents), Max(cents), Sum(cents), Size(1))
        }
        (ticker -> Stats(min, max, sum, size, sum / size))
    })


  def Data = List(
    Stock("FAKE", "2012-01-01", 10000)
  , Stock("FAKE", "2012-01-02", 10020)
  , Stock("FAKE", "2012-01-03", 10022)
  , Stock("FAKE", "2012-01-04", 10005)
  , Stock("FAKE", "2012-01-05",  9911)
  , Stock("FAKE", "2012-01-06",  6023)
  , Stock("FAKE", "2012-01-07",  7019)
  , Stock("FAKE", "2012-01-08",     0)
  , Stock("FAKE", "2012-01-09",  7020)
  , Stock("FAKE", "2012-01-10",  7020)
  , Stock("CAKE", "2012-01-01",     1)
  , Stock("CAKE", "2012-01-02",     2)
  , Stock("CAKE", "2012-01-03",     3)
  , Stock("CAKE", "2012-01-04",     4)
  , Stock("CAKE", "2012-01-05",     5)
  , Stock("CAKE", "2012-01-06",     6)
  , Stock("CAKE", "2012-01-07",     7)
  , Stock("BAKE", "2012-01-01", 99999)
  , Stock("BAKE", "2012-01-02", 99999)
  , Stock("BAKE", "2012-01-03", 99999)
  , Stock("BAKE", "2012-01-04", 99999)
  , Stock("BAKE", "2012-01-05", 99999)
  , Stock("BAKE", "2012-01-06",     0)
  , Stock("BAKE", "2012-01-07", 99999)
  , Stock("LAKE", "2012-01-01", 10012)
  , Stock("LAKE", "2012-01-02",  7000)
  , Stock("LAKE", "2012-01-03",  1234)
  , Stock("LAKE", "2012-01-04",    10)
  , Stock("LAKE", "2012-01-05",  6000)
  , Stock("LAKE", "2012-01-06",  6099)
  , Stock("LAKE", "2012-01-07",  5999)
  )
}


object MonoidTupleInstances {

  /** A monoid for Tuple2 that merges each element with op */
  implicit def Tuple2Monoid[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def identity =
        (Monoid[A].identity, Monoid[B].identity)

      def op(x: (A, B), y: (A, B)) =
        (x, y) match {
          case ((a1, b1), (a2, b2)) =>
            (Monoid[A].op(a1, a2), Monoid[B].op(b1, b2))
        }
    }

  /** A monoid for Tuple3 that merges each element with op */
  implicit def Tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] =
    new Monoid[(A, B, C)] {
      def identity =
        (Monoid[A].identity, Monoid[B].identity, Monoid[C].identity)

      def op(x: (A, B, C), y: (A, B, C)) =
        (x, y) match {
          case ((a1, b1, c1), (a2, b2, c2)) =>
            (Monoid[A].op(a1, a2), Monoid[B].op(b1, b2), Monoid[C].op(c1, c2))
        }
    }

  /** A monoid for Tuple4 that merges each element with op */
  implicit def Tuple4Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid]: Monoid[(A, B, C, D)] =
    new Monoid[(A, B, C, D)] {
      def identity =
        (Monoid[A].identity, Monoid[B].identity, Monoid[C].identity, Monoid[D].identity)

      def op(x: (A, B, C, D), y: (A, B, C, D)) =
        (x, y) match {
          case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
            (Monoid[A].op(a1, a2), Monoid[B].op(b1, b2), Monoid[C].op(c1, c2), Monoid[D].op(d1, d2))
        }
    }

  /** A monoid for Tuple5 that merges each element with op */
  implicit def Tuple5Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid, E: Monoid]: Monoid[(A, B, C, D, E)] =
    new Monoid[(A, B, C, D, E)] {
      def identity =
        (Monoid[A].identity, Monoid[B].identity, Monoid[C].identity, Monoid[D].identity, Monoid[E].identity)

      def op(x: (A, B, C, D, E), y: (A, B, C, D, E)) =
        (x, y) match {
          case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
            (Monoid[A].op(a1, a2), Monoid[B].op(b1, b2), Monoid[C].op(c1, c2), Monoid[D].op(d1, d2), Monoid[E].op(e1, e2))
        }
    }

  /** A monoid for Tuple6 that merges each element with op */
  implicit def Tuple6Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid, E: Monoid, F: Monoid]: Monoid[(A, B, C, D, E, F)] =
    new Monoid[(A, B, C, D, E, F)] {
      def identity =
        (Monoid[A].identity, Monoid[B].identity, Monoid[C].identity, Monoid[D].identity, Monoid[E].identity, Monoid[F].identity)

      def op(x: (A, B, C, D, E, F), y: (A, B, C, D, E, F)) =
        (x, y) match {
          case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
            (Monoid[A].op(a1, a2), Monoid[B].op(b1, b2), Monoid[C].op(c1, c2), Monoid[D].op(d1, d2), Monoid[E].op(e1, e2), Monoid[F].op(f1, f2))
        }
    }

}
