package com.rockthejvm.part2afp

object FunctionalCollections {

  // sets are functions A => Boolean
  val aSet: Set[String] = Set("I", "love", "Scala")
  val setContainsScala = aSet("Scala") // true

  // Seq extends PartialFunctions[Int => A]
  val aSeq: Seq[Int] = Seq(1,2,3,4)
  val anElement = aSeq(2) // 3
//  val aNonExistingElement = aSeq(100) // throw an OOBException

  // Map extends PartialFucntion[K => V]
  val aPhoneBook: Map[String, Int] = Map(
    "Alice" -> 123456,
    "Bob" -> 987654,
  )
  val alicesPhone = aPhoneBook("Alice")

  def main(args: Array[String]): Unit = {

  }
}
