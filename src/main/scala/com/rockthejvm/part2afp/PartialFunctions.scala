package com.rockthejvm.part2afp

object PartialFunctions {
  val aFunction: Int => Int = x => x + 1
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 99
    else throw new RuntimeException("no suitable case possible")

  val aFussyFunction_v2 = (x: Int) => x match
    case 1 => 42
    case 2 => 56
    case 5 => 999

  // partial function
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  val canCallOn37 = aPartialFunction.isDefinedAt(37)
  val liftedPF = aPartialFunction.lift
  val anotherPF: PartialFunction[Int, Int] = {
    case 45 => 86
  }
  val pfChain = aPartialFunction.orElse[Int, Int](anotherPF)

  // HOFs accepts PFs as arguments
  val aList = List(1,2,3,4)
  val aChangedList = aList.map(x => x match
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  )
  val aChangedList_v2 = aList.map({ // possible because PartialFunction[A,B] extends Function1[A,B]
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  })
  val aChangedList_v3 = aList.map {
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  }

  case class Person(name: String, age: Int)
  val someKids = List(
    Person("Alice", 3),
    Person("Bobbie", 5),
    Person("Jane", 4),
  )
  val kidsGrowingUp = someKids.map {
    case Person(name, age) => Person(name, age + 1)
  }

  def main(args: Array[String]): Unit = {
    println(aPartialFunction(2))
//    println(aPartialFunction(33))
    println(liftedPF(2)) // Some(56)
    println(liftedPF(37)) // None
    println(pfChain(45))
  }
}
