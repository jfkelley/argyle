package com.joefkelley

import scala.util.{Try, Success, Failure}
import argyle.reader._
import scala.annotation.tailrec
import scala.language.implicitConversions
import shapeless._
import shapeless.ops.hlist._
import argyle.NonEmptyList

package object argyle {
  
  def repeated[A : Reader](keys: String*): Arg[List[A]] = {
    new NamedArgumentParser[List[A]](keys.toSet, Nil, { xs => sequence(xs.map(implicitly[Reader[A]].apply)) })
  }
  
  def repeatedAtLeastOnce[A : Reader](keys: String*): Arg[List[A]] = repeated[A](keys:_*).flatMap {
    case Nil => Failure(new Error(s"Missing value for required arg ${keys.mkString("/")}"))
    case xs => Success(xs)
  }
  
  def required[A : Reader](keys: String*): Arg[A] = repeated[A](keys:_*).flatMap {
    case List(x) => Success(x)
    case Nil => Failure(new Error(s"Missing value for required arg ${keys.mkString("/")}"))
    case _ => Failure(new Error(s"More than one value for arg ${keys.mkString("/")}"))
  }

  def optional[A : Reader](keys: String*): Arg[Option[A]] = repeated[A](keys:_*).flatMap {
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
  
  def requiredFree[A : Reader]: Arg[A] = optionalFree[A].flatMap {
    case None => Failure(new Error("Missing required argument"))
    case Some(a) => Success(a)
  }
  
  def optionalFree[A : Reader]: Arg[Option[A]] = new SingleFreeArg(implicitly[Reader[A]].apply)
  
  def repeatedFree[A : Reader]: Arg[List[A]] = RepeatedFreeArg(implicitly[Reader[A]].apply)
  
  def repeatedAtLeastOnceFree[A : Reader]: Arg[List[A]] = repeatedFree[A].flatMap {
    case Nil => Failure(new Error("Missing required argument"))
    case xs => Success(xs)
  }
  
  def flag(keys: String*): Arg[Boolean] = optionalOneOf(keys.map(_ -> true):_*).default(false)
  
  def optionalBranch[A](kvs: (String, Arg[A])*): Arg[Option[A]] = BranchArg[A](kvs.toMap)
  
  def requiredBranch[A](kvs: (String, Arg[A])*): Arg[A] = optionalBranch(kvs:_*).flatMap {
    case Some(a) => Success(a)
    case None => Failure(new Error(s"Missing required branching argument: one of ${kvs.map(_._1).mkString(", ")}"))
  }
  
  def constant[A](a: A): Arg[A] = new Arg[A] {
    override def visit(xs: NonEmptyList[String], mode: ArgMode) = Seq(VisitNoop)
    override def complete = Success(a)
  }
  
  def combine[A, B, C](arg1: Arg[A], arg2: Arg[B], combineFinish: (Try[A], Try[B]) => Try[C]): Arg[C] = new Arg[C] {
    override def visit(xs: NonEmptyList[String], mode: ArgMode): Seq[VisitResult[C]] = {
      val opts1 = arg1.visit(xs, mode)
      opts1.flatMap {
        case v: VisitError => Seq(v)
        case VisitConsume(next1, remaining) => Seq(VisitConsume(combine(next1, arg2, combineFinish), remaining))
        case VisitNoop => arg2.visit(xs, mode).map {
          case v: VisitError => v
          case VisitConsume(next2, remaining) => VisitConsume(combine(arg1, next2, combineFinish), remaining)
          case VisitNoop => VisitNoop
        }
      }
    }
    
    def complete: Try[C] = combineFinish(arg1.complete, arg2.complete)
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
  
  private[argyle] def sequence[A](xs: List[Try[A]]): Try[List[A]] = {
    xs.foldLeft[Try[List[A]]](Success(List.empty[A])) { case (xst, yt) =>
      for {
        xs <- xst
        y <- yt
      } yield y :: xs
    }.map(_.reverse)
  }
}