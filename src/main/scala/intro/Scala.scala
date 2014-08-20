package course

/*
 * The following is a high level guide of what scala feautes we
 * need to understand to get started.
 */
object Scala {

  /* sbt & repl */
  // val name = "Leo"
  // val name: String = "Leo"

  // println("hello")

  // def double(n: Int) = n * 2
  // def double(n: Int): Int = n * 2

  // val square: Int => Int = n => n * n
  // val square = (n: Int) => n * n

  // val numbers = List(1,2,3,4)

  // numbers.map(double)

  // numbers.foldRight(0)(_+_)

  // numbers.map(square compose double)

  // /* classes & objects */
  // class SimpleClass {
  //   def sayHello(name: String) =
  //     println(s"hello ${name}")
  // }

  // new SimpleClass().sayHello("people")

  // object SimpleObject {
  //   def sayHello(name: String) =
  //     println(s"hello ${name}")
  // }

  // SimpleObject.sayHello("people")

  // /* methods, values  */

  // /* procedures (don't) */
  // def myProcedure() {
  //   println("I don't return anything")
  // }
  // myProcedure()

  // /* parameter lists curried / tupled */
  // def greeter(greeting: String)(name: String) =
  //   println(s"${greeting}, ${name}!")

  // val myGreeter = greeter("Oh hai") _
  // myGreeter("Leo")

  // /* functions */

  // /* parametricity */
  // def reverse[A](xs: List[A]): List[A] =
  //   xs.foldLeft(Nil : List[A])((acc,x) => x :: acc)

  // reverse(List(1,2,3,4))

  // // Things to note:
  // // - Every element in the input list appears in the output list

  // /* case classes - product types  */
  // sealed case class RGBColor( red :Int, green :Int, blue :Int )

  // /* case classes - sum types */
  // sealed abstract class Color
  // case object Red extends Color
  // case object Green extends Color
  // case object Blue extends Color

  // /* pattern matching */

  // List(1,2,3,4) match {
  //   case x :: xs => x
  //   case _       => "error"
  // }

  // val n: Option[Int] = Some(1)

  // n match {
  //   case Some(n) => n + 1
  //   case None    => "error"
  // }

  /* there is lots more, for comprehensions, implicits, laziness / call-by-name,
    ... but this is enough to get started */
}
