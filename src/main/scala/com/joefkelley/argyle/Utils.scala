package com.joefkelley.argyle

import scala.util.{Try, Success}

object Utils {
  def sequence[A](xs: List[Try[A]]): Try[List[A]] = {
    xs.foldLeft[Try[List[A]]](Success(List.empty)) { case (xst, yt) =>
      for {
        xs <- xst
        y <- yt
      } yield y :: xs
    }.map(_.reverse)
  }
}