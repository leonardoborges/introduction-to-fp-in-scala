package intro

import org.scalacheck._, Arbitrary._, Gen._, Prop._

object ListSpecification extends Properties("List") {
  /*
   * Example: Verify that that our Lists.length matches the
   * builtin List#size
   */
  property("Lists#length matches standard library") =
    forAll((xs: List[Int]) => Lists.length(xs) == xs.size)

  /* Exercise 1 */
  property("Lists#append - the length of the result is equal to the sum of the two input lengths") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.length(xs) + Lists.length(ys) == Lists.length(Lists.append(xs, ys)))

  /* Exercise 2 */
  property("Lists#append - every element in the first and second list appears in the result") =
    forAll((xs: List[Int], ys: List[Int]) => {
      val result = Lists.append(xs, ys)
      xs.foldRight(true)((x, acc) => acc && result.contains(x)) && ys.foldRight(true)((y, acc) => acc && result.contains(y))
    })

  /* Exercise 3 */
  property("Lists#filter - filter(_ => false) always gives empty list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => false) == Nil)

  /* Exercise 4 */
  property("Lists#filter - filter(_ => true) always gives input list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => true) == xs)

  /* Exercise 5 */
  property("Lists#filter - length of output is always less than length of input") =
    forAll((xs: List[Int], pred: Int => Boolean) => Lists.filter(xs)(pred).size <= xs.size)

  /* *Challenge* exercise 6.1
     Identify a set of properties that together with the type signature
     guarantees the validity of your reverse function (assuming pure-total FP) */
  property("Lists#reverse - reversing list twice gives the input list") =
    forAll((xs: List[Int]) => Lists.reverse(Lists.reverse(xs)) == xs)

  /* *Challenge* exercise 6.2
     Identify a set of properties for testing sequence */
  property("Lists#reverse - every element in the input list is in the output list") =
    forAll((xs: List[Int]) => Lists.reverse(xs).foldRight(true)(xs.contains(_) && _))


  /* *Challenge* exercise 8
     Identify a set of properties for testing ranges */
  property("Lists#reverse - reversing ys, reversing xs and appending the two lists gives the same as appending ys to xs and reversing the result") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.append(Lists.reverse(ys), Lists.reverse(xs)) == Lists.reverse(Lists.append(xs, ys)))

  /* *Challenge* exercise 7
     Identify a set of properties for testing sequence */
  property("Lists#sequence - result contains xs") =
    forAll((xs: List[Int]) => Lists.sequence(xs.map(Some(_))) match {
      case Some(result) => result == xs
    })

  /* *Challenge* exercise 8
     Identify a set of properties for testing ranges */
  property("Lists#ranges...") =
    ???

}
