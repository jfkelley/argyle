package com.joefkelley.argyle

import scala.util.{Try, Success, Failure}

package object reader {
  trait Reader[A] {
    def apply(s: String): Try[A]
  }
  
  implicit val StrParser =     new Reader[String]  { def apply(s: String) = Success(s) }
  implicit val BooleanParser = new Reader[Boolean] { def apply(s: String) = Try(s.toBoolean) }
  implicit val ByteParser =    new Reader[Byte]    { def apply(s: String) = Try(s.toByte) }
  implicit val ShortParser =   new Reader[Short]   { def apply(s: String) = Try(s.toShort) }
  implicit val IntParser =     new Reader[Int]     { def apply(s: String) = Try(s.toInt) }
  implicit val LongParser =    new Reader[Long]    { def apply(s: String) = Try(s.toLong) }
  implicit val FloatParser =   new Reader[Float]   { def apply(s: String) = Try(s.toFloat) }
  implicit val DoubleParser =  new Reader[Double]  { def apply(s: String) = Try(s.toDouble) }
  implicit val CharParser =    new Reader[Char] {
    def apply(s: String) = {
      if (s.length == 1) Success(s.head)
      else Failure(new Error(s"Couldn't convert $s to single character"))
    }
  }
  
  implicit def eitherParser[A, B](implicit ap: Reader[A], bp: Reader[B]): Reader[Either[A, B]] = {
    new Reader[Either[A, B]] {
      def apply(s: String): Try[Either[A, B]] = ap(s) match {
        case Success(a) => Success(Left(a))
        case Failure(e1) => bp(s) match {
          case Success(b) => Success(Right(b))
          case Failure(e2) => Failure(e2)
        }
      }
    }
  }
  
  implicit def listParser[A](implicit ap: Reader[A]): Reader[List[A]] = new Reader[List[A]] {
    def apply(s: String): Try[List[A]] = {
      if (s.isEmpty()) Success(List.empty[A]) else Utils.sequence(s.split(",").toList.map(ap.apply))
    }
  }
}