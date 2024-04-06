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
}

case class Empty[A]() extends FSet[A] {
   // TODO

  override def contains(elem: A): Boolean = false
  override infix def +(elem: A): FSet[A] = Cons(elem, this)
  override infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet

  override def map[B](f: A => B): FSet[B] = Empty()
  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()
  override def filter(predicate: A => Boolean): FSet[A] = this
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

  }
}
