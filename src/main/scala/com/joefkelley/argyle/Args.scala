package com.joefkelley.argyle

import com.joefkelley.argyle._
import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec;
import shapeless.HList
import shapeless.ops.hlist._

sealed abstract class VisitResult[+A]
case class VisitError(msg: String) extends VisitResult[Nothing]
case class VisitConsume[+A](next: Arg[A], remaining: List[String]) extends VisitResult[A]
object VisitNoop extends VisitResult[Nothing]

trait Arg[+A] {
  
  def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[A]
  
  def complete: Try[A]
  
}

private[argyle] case class NamedArgumentParser[A]
  (names: Set[String], collected: List[String], completeF: List[String] => Try[A]) extends Arg[A] {
  
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[A] = mode match {
    case SpaceSeparated => {
      xs match {
        case NonEmptyList(first, rest1) if names.contains(first) => rest1 match {
          case second :: rest2 => VisitConsume(this.copy(collected = collected :+ second), rest2)
          case Nil => VisitError("Missing argument for key " + first)
        }
        case _ => VisitNoop
      }
    }
    case EqualsSeparated => {
      xs match {
        case NonEmptyList(first, rest) => names.find(n => first.startsWith(n + '=')) match {
          case Some(name) => VisitConsume(this.copy(collected = collected :+ first.drop(name.size + 1)), rest)
          case None => VisitNoop
        }
      }
    }
  }
  
  override def complete: Try[A] = completeF(collected)
}

private[argyle] case class OptionMapParser[A](map: Map[String, A]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = xs match {
    case NonEmptyList(head, rest) => map.get(head) match {
      case Some(a) => VisitConsume(new Arg[Some[A]] {
        override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Some[A]] = {
          xs match {
            case NonEmptyList(h2, rest) if map.contains(head) => {
              if (h2 == head) {
                VisitError(s"Argument $head is present more than once")
              } else {
                VisitError(s"Conflicting arguments $head and $h2")
              }
            }
            case _ => VisitNoop
          }
        }
        override def complete: Try[Some[A]] = Success(Some(a))
      }, rest)
      case None => VisitNoop
    }
  }
  override def complete: Try[Option[A]] = Success(None)
}

private[argyle] case class SingleFreeArg[A](parse: String => Try[A]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = {
    if (!free) {
      VisitNoop
    } else xs match {
      case NonEmptyList(head, rest) => VisitConsume(new Arg[Option[A]] {
        override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode) = VisitNoop
        override def complete = parse(head).map(Some.apply)
      }, rest)
    }
  }
  override def complete: Try[Option[A]] = Success(None)
}

private[argyle] case class RepeatedFreeArg[A](
    parse: String => Try[A], collected: List[String] = Nil) extends Arg[List[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[List[A]] = {
    if (!free) {
      VisitNoop
    } else xs match {
      case NonEmptyList(head, rest) => VisitConsume(this.copy(collected = collected :+ head), rest)
    }
  }
  override def complete: Try[List[A]] = Utils.sequence(collected.map(parse))
}

private[argyle] case class BranchArg[A](branch: Map[String, Arg[A]]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = xs match {
    case NonEmptyList(x, rest) => branch.get(x) match {
      case Some(arg) => VisitConsume(new PostProcess(arg).map(a => Some(a)), rest)
      case None => VisitNoop
    }
  }
  override def complete: Try[Option[A]] = Success(None)
}