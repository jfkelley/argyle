package com.joefkelley.argyle

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import java.time.LocalTime
import java.time.LocalDateTime

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
  implicit val FileParser = new Reader[File] { def apply(s: String) = Success(new File(s)) }
  implicit val PathParser = new Reader[Path] { def apply(s: String) = Success(Paths.get(s)) }
  implicit val ScalaDurationParser = new Reader[scala.concurrent.duration.Duration] {
    def apply(s: String) = Try(scala.concurrent.duration.Duration(s))
  }
  implicit val JavaDurationParser = new Reader[java.time.Duration] {
    def apply(s: String) = Try(java.time.Duration.parse(s))
  }
  
  implicit val DateParser = new Reader[LocalDate] {
    def apply(s: String) = Try(LocalDate.parse(s, DateTimeFormatter.ISO_DATE))
  }
  implicit val TimeParser = new Reader[LocalTime] {
    def apply(s: String) = Try(LocalTime.parse(s, DateTimeFormatter.ISO_TIME))
  }
  implicit val DateTimeParser = new Reader[LocalDateTime] {
    def apply(s: String) = Try(LocalDateTime.parse(s, DateTimeFormatter.ISO_LOCAL_DATE_TIME))
  }
  
  implicit def eitherParser[A : Reader, B : Reader]: Reader[Either[A, B]] = {
    new Reader[Either[A, B]] {
      def apply(s: String): Try[Either[A, B]] = implicitly[Reader[A]].apply(s) match {
        case Success(a) => Success(Left(a))
        case Failure(e1) => implicitly[Reader[B]].apply(s) match {
          case Success(b) => Success(Right(b))
          case Failure(e2) => Failure(e2)
        }
      }
    }
  }
  
  implicit def listParser[A : Reader]: Reader[List[A]] = new Reader[List[A]] {
    def apply(s: String): Try[List[A]] = {
      if (s.isEmpty()) Success(List.empty[A]) else Utils.sequence(s.split(",").toList.map(implicitly[Reader[A]].apply))
    }
  }
}