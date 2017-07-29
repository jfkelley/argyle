package com.joefkelley

import scala.util.{Try, Success, Failure}
import argyle.reader._
import scala.annotation.tailrec
import scala.language.implicitConversions
import shapeless._
import shapeless.ops.hlist._
import argyle.NonEmptyList

package object argyle {
  
  def repeated[A](keys: String*)(implicit p: Reader[A]): Arg[List[A]] = {
    new NamedArgumentParser[List[A]](keys.toSet, Nil, { xs => sequence(xs.map(p.apply)) })
  }
  
  def repeatedAtLeastOnce[A](keys: String*)(implicit p: Reader[A]): Arg[List[A]] = repeated[A](keys:_*).flatMap {
    case Nil => Failure(new Error(s"Missing value for required arg ${keys.mkString("/")}"))
    case xs => Success(xs)
  }
  
  def required[A](keys: String*)(implicit p: Reader[A]): Arg[A] = repeated[A](keys:_*).flatMap {
    case List(x) => Success(x)
    case Nil => Failure(new Error(s"Missing value for required arg ${keys.mkString("/")}"))
    case _ => Failure(new Error(s"More than one value for arg ${keys.mkString("/")}"))
  }

  def optional[A](keys: String*)(implicit p: Reader[A]): Arg[Option[A]] = repeated[A](keys:_*).flatMap {
    case List(x) => Success(Some(x))
    case Nil => Success(None)
    case _ => Failure(new Error(s"More than one value for arg ${keys.mkString("/")}"))
  }
  
  def optionalOneOf[A](kvs: (String, A)*): Arg[Option[A]] = {
    new OptionMapParser[A](kvs.toMap)
  }
  
  def requiredOneOf[A](kvs: (String, A)*): Arg[A] = {
    new OptionMapParser[A](kvs.toMap).flatMap {
      case Some(a) => Success(a)
      case None => Failure(new Error(s"Missing required argument: one of ${kvs.map(_._1).mkString(", ")}"))
    }
  }
  
  def requiredFree[A](implicit p: Reader[A]): Arg[A] = optionalFree(p).flatMap {
    case None => Failure(new Error("Missing required argument"))
    case Some(a) => Success(a)
  }
  
  def optionalFree[A](implicit p: Reader[A]): Arg[Option[A]] = new SingleFreeArg(p.apply)
  
  def repeatedFree[A](implicit p: Reader[A]): Arg[List[A]] = RepeatedFreeArg(p.apply)
  
  def repeatedAtLeastOnceFree[A](implicit p: Reader[A]): Arg[List[A]] = repeatedFree[A].flatMap {
    case Nil => Failure(new Error("Missing required argument"))
    case xs => Success(xs)
  }
  
  def flag(keys: String*): Arg[Boolean] = optionalOneOf(keys.map(_ -> true):_*).default(false)
  
  def optionalBranch[A](kvs: (String, Arg[A])*): Arg[Option[A]] = BranchArg[A](kvs.toMap)
  
  def requiredBranch[A](kvs: (String, Arg[A])*): Arg[A] = optionalBranch(kvs:_*).flatMap {
    case Some(a) => Success(a)
    case None => Failure(new Error(s"Missing required branching argument: one of ${kvs.map(_._1).mkString(", ")}"))
  }
  
  implicit class Default[A](arg: Arg[Option[A]]) {
    def default(a: A): Arg[A] = arg.map(_.getOrElse(a))
  }
  
  def constant[A](a: A): Arg[A] = new Arg[A] {
    override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode) = VisitNoop
    override def complete = Success(a)
  }
  
  implicit class PostProcess[A](arg: Arg[A]) {
    def flatMap[B](f: A => Try[B]): Arg[B] = new Arg[B] {
      override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[B] = {
        arg.visit(xs, free, mode) match {
          case v: VisitError => v
          case VisitNoop => VisitNoop
          case VisitConsume(next, remaining) => VisitConsume(next.flatMap(f), remaining)
        }
      }
      
      override def complete: Try[B] = arg.complete.flatMap(f)
    }
    def map[B](f: A => B): Arg[B] = flatMap(a => Success(f(a)))
    def as[B](implicit f: A => B): Arg[B] = map(f)
  }
  
  def combine[A, B, C](arg1: Arg[A], arg2: Arg[B], combineFinish: (Try[A], Try[B]) => Try[C]): Arg[C] = new Arg[C] {
    override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[C] = {
      arg1.visit(xs, free, mode) match {
        case v: VisitError => v
        case VisitConsume(next1, remaining) => VisitConsume(combine(next1, arg2, combineFinish), remaining)
        case VisitNoop => {
          arg2.visit(xs, free, mode) match {
            case v: VisitError => v
            case VisitConsume(next2, remaining) => VisitConsume(combine(arg1, next2, combineFinish), remaining)
            case VisitNoop => VisitNoop
          }
        }
      }
    }
    
    def complete: Try[C] = combineFinish(arg1.complete, arg2.complete)
  }
  
  implicit class ArgSum[A](arg1: Arg[A]) {
    
    def or(arg2: Arg[A], f: (A, A) => A): Arg[A] = combine(arg1, arg2, { (t1: Try[A], t2: Try[A]) => (t1, t2) match {
      case (Success(a1), Success(a2)) => Success(f(a1, a2))
      case (Success(a1), _) => Success(a1)
      case (_, Success(a2)) => Success(a2)
      case (Failure(e1), Failure(e2)) => Failure(new Error("Neither side of OR was matched", e2))
    }})
    
    def xor(arg2: Arg[A]): Arg[A] = combine(arg1, arg2, { (t1: Try[A], t2: Try[A]) => (t1, t2) match {
      case (Success(a1), Success(a2)) => Failure(new Error("Both sides of XOR were matched"))
      case (Success(a1), _) => Success(a1)
      case (_, Success(a2)) => Success(a2)
      case (Failure(e1), Failure(e2)) => Failure(new Error("Neither side of OR was matched", e2))
    }})
    
  }
  
  implicit class ArgProduct[A <: HList](arg1: Arg[A]) {
    def and[B](arg2: Arg[B])(implicit p: Prepend[A, B :: HNil]): Arg[p.Out] = combine(arg1, arg2, {
      (at: Try[A], bt: Try[B]) => for (a <- at; b <- bt) yield p(a, b :: HNil)
    })
  }
  
  implicit def promoteAndMakeProduct[A](arg: Arg[A]): ArgProduct[A :: HNil] = new ArgProduct(arg.map(_ :: HNil))
  
  trait GenericHListFunction[A <: HList, B] {
    def apply(a: A): B
  }
  implicit def genericToF[A <: HList, B](implicit gen: Generic.Aux[B, A]): GenericHListFunction[A, B] = 
    new GenericHListFunction[A, B] { def apply(a: A): B = gen.from(a) }
  
  implicit class GenericOut[A <: HList](arg: Arg[A]) {
    def to[B](implicit gen: GenericHListFunction[A, B]): Arg[B] = arg.map(gen.apply)
  }
  
  implicit def promoteAndMakeGenericOut[A](arg: Arg[A]): GenericOut[A :: HNil] = new GenericOut(arg.map(_ :: HNil))
  
  implicit class Parser[A](arg: Arg[A]) {
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
      _parse(arg, xs)
    }
    def parse(xs: Array[String], mode: ArgMode = SpaceSeparated): Try[A] = parse(xs.toList, mode)
    def parse(xs: String*): Try[A] = parse(xs.toArray)
  }
  
  private[argyle] def sequence[A](xs: List[Try[A]]): Try[List[A]] = {
    xs.foldLeft[Try[List[A]]](Success(List.empty[A])) { case (xst, yt) =>
      for {
        xs <- xst
        y <- yt
      } yield y :: xs
    }.map(_.reverse)
  }
}