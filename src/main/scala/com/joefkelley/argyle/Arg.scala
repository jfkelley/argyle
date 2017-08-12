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
  
  
  def flatMap[B](f: A => Try[B]): Arg[B] = {
    val current = this
    new Arg[B] {
      override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[B] = {
        current.visit(xs, free, mode) match {
          case v: VisitError => v
          case VisitNoop => VisitNoop
          case VisitConsume(next, remaining) => VisitConsume(next.flatMap(f), remaining)
        }
      }
      override def complete: Try[B] = current.complete.flatMap(f)
    }
  }
  def map[B](f: A => B): Arg[B] = flatMap(a => Success(f(a)))
  def as[B](implicit f: A => B): Arg[B] = map(f)
  
  def default[B](b: B)(implicit ev: A <:< Option[B]): Arg[B] = map(a => ev(a).getOrElse(b))
  
  def or[B >: A](arg2: Arg[B], f: (B, B) => B): Arg[B] = combine(this, arg2, { (t1: Try[B], t2: Try[B]) =>
    (t1, t2) match {
      case (Success(b1), Success(b2)) => Success(f(b1, b2))
      case (Success(b1), _) => Success(b1)
      case (_, Success(b2)) => Success(b2)
      case (Failure(e1), Failure(e2)) => Failure(new Error("Neither side of OR was matched", e2))
    }
  })
    
  def xor[B >: A](arg2: Arg[B]): Arg[B] = combine(this, arg2, { (t1: Try[B], t2: Try[B]) =>
    (t1, t2) match {
      case (Success(b1), Success(b2)) => Failure(new Error("Both sides of XOR were matched"))
      case (Success(b1), _) => Success(b1)
      case (_, Success(b2)) => Success(b2)
      case (Failure(e1), Failure(e2)) => Failure(new Error("Neither side of OR was matched", e2))
    }
  })
  
  def parse(xs: List[String], mode: ArgMode): Try[A] = {
    @tailrec
    def _parse(currentArg: Arg[A], currentArgs: List[String]): Try[A] = NonEmptyList(currentArgs) match {
      case None => currentArg.complete
      case Some(nonEmpty) => currentArg.visit(nonEmpty, false, mode) match {
        case VisitError(msg) => Failure(new Error(msg))
        case VisitConsume(next, remaining) => _parse(next, remaining)
        case VisitNoop => currentArg.visit(nonEmpty, true, mode) match {
          case VisitError(msg) => Failure(new Error(msg))
          case VisitConsume(next, remaining) => _parse(next, remaining)
          case VisitNoop => Failure(new Error("Unused argument: " + xs.head))
        }
      }
    }
    _parse(this, xs)
  }
  def parse(xs: Array[String], mode: ArgMode = SpaceSeparated): Try[A] = parse(xs.toList, mode)
  def parse(xs: String*): Try[A] = parse(xs.toArray)
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

private[argyle] case class OptionMapParser[A](optionMap: Map[String, A]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = xs match {
    case NonEmptyList(head, rest) => optionMap.get(head) match {
      case Some(a) => VisitConsume(new Arg[Some[A]] {
        override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Some[A]] = {
          xs match {
            case NonEmptyList(h2, rest) if optionMap.contains(h2) => {
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

private[argyle] case class SingleFreeArg[A](read: String => Try[A]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = {
    if (!free) {
      VisitNoop
    } else xs match {
      case NonEmptyList(head, rest) => VisitConsume(new Arg[Option[A]] {
        override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode) = VisitNoop
        override def complete = read(head).map(Some.apply)
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
      case Some(arg) => VisitConsume(arg.map(a => Some(a)), rest)
      case None => VisitNoop
    }
  }
  override def complete: Try[Option[A]] = Success(None)
}