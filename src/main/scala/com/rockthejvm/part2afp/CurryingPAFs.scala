package com.rockthejvm.part2afp

object CurryingPAFs {
  // currying
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3: Int => Int = superAdder(3)
  val eight = add3(5)
  val eight_v2 = superAdder(3)(5)

  // curried methods
  def curriedAdder(x: Int)(y: Int): Int =
    x + y

  // methods != function values

  // converting methods to functions = eta-expansion
  val add4 = curriedAdder(4) // eta-expansion
  val nine = add4(5) // 9

  def increment(x: Int): Int = x + 1
  val aList = List(1,2,3)
  val anIncrementedList = aList.map(increment) // eta-expansion

  // underscores are powerful
  def concatenator(a: String, b: String, c: String): String = a + b + c
  val insertName = concatenator(
    "Hello, my name is ",
    _: String,
    ", I'm going to show you a nice Scala trick."
  ) // x => concatenator("...", x, "...")

  val danielsGreeting = insertName("Daniel")
  val fillInTheBlanks = concatenator(_: String, "Daniel", _:String)
  val danielsGreeting_v2 = fillInTheBlanks("Hi", "how are you?")

  /**
   * Exercises
   */
  def simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  // 1 - obtain an add7 function: x => x + 7 out of these 3 definitions
  val add7 = (x: Int) => simpleAddMethod(x, 7)
  val add7_v2 = simpleAddMethod(_: Int, 7)
  val add7_v3 = simpleAddFunction(_: Int, 7)
  val add7_v4 = (x: Int) => simpleAddFunction(x, 7)
  val add7_v5 = curriedMethod(_: Int)(7)
  val add7_v6 = (x: Int) => curriedMethod(x)(7)
  val add7_v7 = simpleAddMethod.curried(7)

  // 2 - process a list of numbers and return their string representation under different formats
  // step 1: create a curried formatting method
  // step 2: process a list of numbers with various formats
  val piWith2Dec = "%4.2f".format(Math.PI) // 3.14
  def formattedNumbers(fmt: String)(number: Double): String = fmt.format(number)
  val newList: List[Double] = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  // methods vs functions + by-name vs 0-lambda
  def byName(n: => Int) = n + 1
  def byLambda(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23) // ok
  byName(method) // eta-expanded? NO - method is invoked here
  byName(parenMethod()) // 43
//  byName(parenMethod) // not ok
  byName((() => 42)())
//  byName(() => 42) // not ok

//  byLambda(23) // not ok
//  byLambda(method) // eta-expansion is NOT possible
  byLambda(parenMethod) // eta-expansion is done
  byLambda(() => 42)
  byLambda(() => parenMethod())
  def main(args: Array[String]): Unit = {

    println(s"${add7(3)}, ${add7_v2(3)}, ${add7_v3(3)}, ${add7_v4(3)}, ${add7_v5(3)}, ${add7_v6(3)}")

    println(newList.map(formattedNumbers("%4.2f")))
    println(newList.map(formattedNumbers("%8.6f")))
    println(newList.map(formattedNumbers("%16.14f")))
  }
}
