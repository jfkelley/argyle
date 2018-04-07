package com.joefkelley.argyle

/*
 * Simple utility class for representing a non-empty List by wrapping a List and restricting access methods
 * so that it can only be constructed and accessed safely.
 * 
 * Technically it is not totally safe in that list.nonEmptyList.get will compile to a NonEmptyList, but could fail
 * at runtime. But Option's .get should be avoided anyway and is the only way to bypass the compiler's checks here.
 */
class NonEmptyList[+A] private (contents: List[A]) {
  require(contents.nonEmpty)
  def head: A = contents.head
  def tail: List[A] = contents.tail
}

object NonEmptyList {
  def apply[A](contents: List[A]): Option[NonEmptyList[A]] = contents match {
    case Nil => None
    case _ => Some(new NonEmptyList(contents))
  }
  
  def unapply[A](list: NonEmptyList[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}