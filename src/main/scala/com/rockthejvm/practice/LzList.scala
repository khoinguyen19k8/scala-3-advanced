package com.rockthejvm.practice

import scala.annotation.tailrec

// Write a lazily evaluated, potentially INFINITE linked list
abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  // utilities
  def #::(element: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A]

  // classics
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A] // takes the first n elements from this lazy list
  def takeAsList(n: Int): List[A] =
    take(n).toList
  def toList: List[A] = // use this carefully on a large lazy list
    @tailrec
    def toListInner(remaining: LzList[A], acc: List[A]): List[A] =
      if remaining.isEmpty then acc.reverse
      else toListInner(remaining.tail, remaining.head :: acc)

    toListInner(this, List())

}

case class LzEmpty[A]() extends LzList[A] {
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException()
  def tail: LzList[A] = throw new NoSuchElementException()

  // utilities
  def #::(element: A): LzList[A] = // prepending
    new LzCons[A](element, this)
  infix def ++(another: => LzList[A]): LzList[A] =
    another

  // classics
  def foreach(f: A => Unit): Unit = ()
  def map[B](f: A => B): LzList[B] = LzEmpty()
  def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty()
  def filter(predicate: A => Boolean): LzList[A] = this

  def take(n: Int): LzList[A] = // takes the first n elements from this lazy list
    if n == 0 then this
    else throw new RuntimeException(s"Cannot take $n elements from an empty lazy list.")
}

class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {
  // hint: use call by need
  def isEmpty: Boolean = false
  override lazy val head: A = hd
  override lazy val tail: LzList[A] = tl

  // utilities
  def #::(element: A): LzList[A] = // prepending
    new LzCons[A](element, this)
  infix def ++(another: => LzList[A]): LzList[A] =
    @tailrec
    def getFinalNode(currentLzList: LzList[A]): LzList[A] =
      if currentLzList.tail.isEmpty then currentLzList
      else getFinalNode(currentLzList.tail)

    val finalNode = getFinalNode(this)
    new LzCons(finalNode.head, another)

  // classics
  def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)
  def map[B](f: A => B): LzList[B] =
    new LzCons(f(head), tail.map(f))
  def flatMap[B](f: A => LzList[B]): LzList[B] = f(head) ++ tail.flatMap[B](f)
  def filter(predicate: A => Boolean): LzList[A] =
    if predicate(head) then new LzCons[A](head, tail.filter(predicate)) // preserve lazy val
    else tail.filter(predicate) // TODO warning

  def take(n: Int): LzList[A] = // takes the first n elements from this lazy list
    if n <= 0 then LzEmpty()
    else if n == 1 then new LzCons(head, LzEmpty())
    else new LzCons(head, tail.take(n - 1)) // preserve lazy eval
}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()
  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons[A](
      start,
      generate(generator(start))(generator)
    )
  def from[A](list: List[A]): LzList[A] =
    list.foldLeft(LzList.empty) { (currentLzList, element) =>
      new LzCons(element, currentLzList)
    }
}

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val naturals = LzList.generate(1)(x => x + 1) // INFINITE list of natural numbers
  }
}
