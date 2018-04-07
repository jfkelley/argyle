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
import java.time.{Duration => jDuration}
import scala.concurrent.duration.Duration

package object reader {
  trait Reader[A] {
    def apply(s: String): Try[A]
  }

  object Reader {
    def summon[A](f : String => Try[A]) : Reader[A] = new Reader[A] {
      override def apply(s: String) = f(s)
    }

    def safeSummon[A](f : String => A) : Reader[A] = new Reader[A] {
      override def apply(s: String) = Try(f(s))
    }
  }
  import Reader._
  
  implicit val StrParser     = summon[String](Success(_))
  implicit val BooleanParser = safeSummon[Boolean](s => s.toBoolean)
  implicit val ByteParser    = safeSummon[Byte](s    => s.toByte)
  implicit val ShortParser   = safeSummon[Short](s   => s.toShort)
  implicit val IntParser     = safeSummon[Int](s     => s.toInt)
  implicit val LongParser    = safeSummon[Long](s    => s.toLong)
  implicit val FloatParser   = safeSummon[Float](s   => s.toFloat)
  implicit val DoubleParser  = safeSummon[Double](s  => s.toDouble)
  implicit val CharParser    = summon[Char]{s =>
    if (s.length == 1) Success(s.head)
    else Failure(new Error(s"Couldn't convert $s to single character"))
  }

  implicit val FileParser = summon[File](s => Success(new File(s)))
  implicit val PathParser = summon[Path](s => Success(Paths.get(s)))

  implicit val ScalaDurationParser = safeSummon[Duration](s  => Duration(s))
  implicit val JavaDurationParser  = safeSummon[jDuration](s => jDuration.parse(s))

  implicit val DateParser = safeSummon[LocalDate](s => LocalDate.parse(s, DateTimeFormatter.ISO_DATE))
  implicit val TimeParser = safeSummon[LocalTime](s => LocalTime.parse(s, DateTimeFormatter.ISO_TIME))

  implicit val DateTimeParser = safeSummon[LocalDateTime]{s =>
    LocalDateTime.parse(s, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  }
  
  implicit def eitherParser[A : Reader, B : Reader] = summon[Either[A, B]]{s =>
    implicitly[Reader[A]].apply(s)
      .map(a => Success(Left[A,B](a)))
      .getOrElse{
        implicitly[Reader[B]].apply(s).map(Right[A,B])
      }
  }
  
  implicit def listParser[A : Reader]: Reader[List[A]] = summon[List[A]]{s =>
    Utils.sequence(s.split(",").filter(_.nonEmpty).toList.map(implicitly[Reader[A]].apply))
  }
}