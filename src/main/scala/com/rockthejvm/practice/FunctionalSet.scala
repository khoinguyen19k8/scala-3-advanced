package com.rockthejvm.practice

import scala.annotation.tailrec

abstract class FSet[A] extends (A => Boolean) {
  // main api
  def contains(elem: A): Boolean
  def apply(elem: A): Boolean = contains(elem)
  infix def +(elem: A): FSet[A]
  infix def ++(anotherSet: FSet[A]): FSet[A]

  // "classics"
  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def foreach(f: A => Unit): Unit

  // methods
  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]

  // "negation" == all the elements of type A EXCEPT the elements in this set
  def unary_! : FSet[A] = new PBSet(x => !contains(x))
}

// example {x in N | x % 2 == 0}
// property-based set
class PBSet[A](property: A => Boolean) extends FSet[A] {
  override def contains(elem: A): Boolean = property(elem)
  override infix def +(elem: A): FSet[A] =
    new PBSet(x => x == elem || property(x))
  override infix def ++(anotherSet: FSet[A]): FSet[A] =
    new PBSet(x => anotherSet(x) || property(x))

  // "classics"
  override def map[B](f: A => B): FSet[B] =
    politelyFail()
  override def flatMap[B](f: A => FSet[B]): FSet[B] =
    politelyFail()
  override def filter(predicate: A => Boolean): FSet[A] =
    new PBSet(x => predicate(x) && property(x))
  override def foreach(f: A => Unit): Unit =
    politelyFail()

  // methods
  override infix def -(elem: A): FSet[A] =
    filter(x => x != elem)
  override infix def --(anotherSet: FSet[A]): FSet[A] =
    filter(!anotherSet)
  override infix def &(anotherSet: FSet[A]): FSet[A] =
    filter(anotherSet)

  // extra utilities (internal)
  private def politelyFail() = throw new RuntimeException("I don't know if this set is iterable...")

}

case class Empty[A]() extends PBSet[A](_ => false) {
  // TODO

  override def map[B](f: A => B): FSet[B] = Empty()
  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()
  override def foreach(f: A => Unit): Unit = ()
}

case class Cons[A](head: A, tail: FSet[A]) extends FSet[A]{
  // TODO

  override def contains(elem: A): Boolean = (head == elem) || tail.contains(elem)

  override infix def +(elem: A): FSet[A] =
    if contains(elem) then this
    else Cons(elem, this)
  override infix def ++(anotherSet: FSet[A]): FSet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)
  override def filter(predicate: A => Boolean): FSet[A] =
    if predicate(head) then tail.filter(predicate) + head
    else tail.filter(predicate)

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override infix def -(elem: A): FSet[A] = filter(_ != elem)

  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)

  override infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)
}

object FSet {
  def apply[A](values: A*): FSet[A] =
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] =
      if valuesSeq.isEmpty then acc
      else buildSet(valuesSeq.tail, acc + valuesSeq.head)

    buildSet(values, Empty())
}
object FunctionalSetPlayground {
  def main(args: Array[String]): Unit = {
    val fooSet: FSet[Int] = FSet(1,2,3)
    val anotherSet: FSet[Int] = FSet(3,5,7)
    val combinedSet: FSet[Int] = fooSet ++ anotherSet
    println(fooSet(2)) // true
    println((fooSet + 4)(4)) // true
    println(combinedSet(7)) // true
    println(fooSet.map(_ * 2)(6)) // true
    println(fooSet.flatMap(x => FSet[Float](x.toFloat / 2))(1.5)) // true
    println(combinedSet.filter(_ % 2 == 0)(5)) // false

    val aSet = Set(1,2,3)
    val aList = (1 to 10).toList
    println(aList.filter(aSet)) // List[1,2,3]

    println((fooSet - 3)(3)) // false
    println((fooSet -- anotherSet)(3)) // false
    println((fooSet & anotherSet)(3)) // true
    println((fooSet & anotherSet)(2)) // false

    val naturals = new PBSet[Int](_ => true)
    println(naturals.contains(5237548)) // true
    println(!naturals.contains(0)) // false
    println((!naturals + 1 + 2 + 3).contains(3)) // true
//    println(!naturals.map(_ + 1)) // throw

  }
}
