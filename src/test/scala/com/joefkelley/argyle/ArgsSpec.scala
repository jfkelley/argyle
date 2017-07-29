package com.joefkelley.argyle

import org.scalatest._
import org.scalatest.matchers._
import com.joefkelley.argyle._
import scala.util.{Failure, Success, Try}

class ArgsSpec extends FlatSpec with Matchers {
  
  "Optional keyed arg" should "be matched when present" in {
    optional[String]("--foo").parse("--foo", "bar") should succeedWith(Option("bar"))
  }
  it should "be empty when not present" in {
    optional[String]("--foo").parse() should succeedWith(None: Option[String])
  }
  it should "fail when key present without value" in {
    optional[String]("--foo").parse("--foo") should fail
  }
  it should "fail when present twice" in {
    optional[String]("--foo").parse("--foo", "bar", "--foo", "baz") should fail
  }
  it should "be matched when in equals sign mode" in {
    optional[String]("--foo").parse(Array("--foo=bar"), EqualsSeparated) should succeedWith(Option("bar"))
  }
  it should "not be matched when missing in equals sign mode" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      requiredFree[Int]
    ).to[Person]
    args.parse(Array("50"), EqualsSeparated) should fail
  }
  
  "Required keyed arg" should "be matched when present" in {
    required[String]("--foo").parse("--foo", "bar") should succeedWith("bar")
  }
  it should "fail when not present" in {
    required[String]("--foo").parse() should fail
  }
  it should "fail when key present without value" in {
    required[String]("--foo").parse("--foo") should fail
  }
  it should "fail when present twice" in {
    required[String]("--foo").parse("--foo", "bar", "--foo", "baz") should fail
  }
  
  "Combined args" should "be converted to case class with two members" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age")
    ).to[Person]
    args.parse("--age", "50", "--name", "foo") should succeedWith(Person("foo", 50))
  }
  it should "be converted to case class with three members" in {
    case class Person(name: String, age: Int, happy: Boolean) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age") and
      flag("--happy")
    ).to[Person]
    args.parse("--name", "foo", "--age", "50") should succeedWith(Person("foo", 50, false))
  }
  it should "be converted to case class with one member" in {
    case class Person(name: String) {}
    val args = (
      required[String]("--name")
    ).to[Person]
    args.parse("--name", "foo") should succeedWith(Person("foo"))
  }
  it should "be nestable" in {
    case class Name(first: String, last: String) {}
    case class Person(name: Name, age: Int) {}
    val args = (
      (
        required[String]("--first") and
        required[String]("--last")
      ).to[Name] and
      required[Int]("--age")
    ).to[Person]
    args.parse("--first", "foo", "--last", "bar", "--age", "50") should succeedWith(Person(Name("foo", "bar"), 50))
  }
  it should "fail when one is missing" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age")
    ).to[Person]
    args.parse("--name", "foo") should fail
  }
  it should "fail when one doesn't parse" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age")
    ).to[Person]
    args.parse("--name", "foo", "--age", "bar") should fail
  }
  
  "Required branch" should "branch" in {
    val args = requiredBranch(
        "--foo" -> required[Int]("--fooN"),
        "--bar" -> required[Int]("--barN")
    )
    args.parse("--foo", "--fooN", "50") should succeedWith(50)
  }
  it should "work with ADT" in {
    sealed trait Vehicle
    case class Car(color: String) extends Vehicle
    case class Boat(size: Int, name: String) extends Vehicle
    
    val args = requiredBranch(
        "--car"  -> required[String]("--color").to[Car],
        "--boat" -> (required[Int]("--size") and required[String]("--name")).to[Boat]
    )
    args.parse("--boat", "--size", "50", "--name", "Boaty") should succeedWith(Boat(50, "Boaty"): Vehicle)
  }
  it should "fail when missing" in {
    val args = requiredBranch(
        "--foo" -> required[Int]("--fooN"),
        "--bar" -> required[Int]("--barN")
    )
    args.parse() should fail
  }
  
  "Optional branch" should "branch" in {
    val args = optionalBranch(
        "--foo" -> required[Int]("--fooN"),
        "--bar" -> required[Int]("--barN")
    )
    args.parse("--foo", "--fooN", "50") should succeedWith(Option(50))
  }
  it should "work with ADT" in {
    sealed trait Vehicle
    case class Car(color: String) extends Vehicle
    case class Boat(size: Int, name: String) extends Vehicle
    
    val args = optionalBranch(
        "--car"  -> required[String]("--color").to[Car],
        "--boat" -> (required[Int]("--size") and required[String]("--name")).to[Boat]
    )
    args.parse("--boat", "--size", "50", "--name", "Boaty") should succeedWith(Option(Boat(50, "Boaty"): Vehicle))
  }
  it should "return None when missing" in {
    val args = optionalBranch(
        "--foo" -> required[Int]("--fooN"),
        "--bar" -> required[Int]("--barN")
    )
    args.parse() should succeedWith(None: Option[Int])
  }
  
  "optionalOneOf arg" should "pick the correct option" in {
    val arg = optionalOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse("--bar") should succeedWith(Option(10))
  }
  it should "return None when no option specified" in {
    val arg = optionalOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse() should succeedWith(None: Option[Int])
  }
  it should "fail when present more than once with same value" in {
    val arg = optionalOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse("--foo", "--foo") should fail
  }
  it should "fail when present more than once with different values" in {
    val arg = optionalOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse("--foo", "--bar") should fail
  }
  
  "requiredOneOf arg" should "fail when missing" in {
    val arg = requiredOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse() should fail
  }
  it should "succeed when present" in {
    val arg = requiredOneOf("--foo" -> 5, "--bar" -> 10)
    arg.parse("--foo") should succeedWith(5)
  }
  
  "repeated arg" should "return empty when not present" in {
    val arg = repeated[Int]("--foo")
    arg.parse() should succeedWith(List.empty[Int])
  }
  it should "return results when present" in {
    val arg = repeated[Int]("--foo")
    arg.parse("--foo", "1", "--foo", "2") should succeedWith(List(1,2))
  }
  
  "repeated at least once arg" should "fail when not present" in {
    val arg = repeatedAtLeastOnce[Int]("--foo")
    arg.parse() should fail
  }
  it should "return results when present" in {
    val arg = repeatedAtLeastOnce[Int]("--foo")
    arg.parse("--foo", "1", "--foo", "2") should succeedWith(List(1,2))
  }
  
  "required free arg" should "succeed when at the end of args" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      requiredFree[Int]
    ).to[Person]
    args.parse("--name", "foo", "50") should succeedWith(Person("foo", 50))
  }
  it should "succeed when at the start of args" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      requiredFree[Int]
    ).to[Person]
    args.parse("50", "--name", "foo") should succeedWith(Person("foo", 50))
  }
  it should "fail when missing" in {
    case class Person(name: String, age: Int) {}
    val args = (
      required[String]("--name") and
      requiredFree[Int]
    ).to[Person]
    args.parse("--name", "foo") should fail
  }
  it should "not take precedence over named arg" in {
    case class Person(name: String, address: String) {}
    val args = (
      requiredFree[String] and
      required[String]("--address")
    ).to[Person]
    args.parse("--address", "foo", "bar") should succeedWith(Person("bar", "foo"))
  }
  
  "optional free arg" should "succeed when at the end of args" in {
    case class Person(name: String, age: Option[Int]) {}
    val args = (
      required[String]("--name") and
      optionalFree[Int]
    ).to[Person]
    args.parse("--name", "foo", "50") should succeedWith(Person("foo", Some(50)))
  }
  it should "succeed when at the start of args" in {
    case class Person(name: String, age: Option[Int]) {}
    val args = (
      required[String]("--name") and
      optionalFree[Int]
    ).to[Person]
    args.parse("50", "--name", "foo") should succeedWith(Person("foo", Some(50)))
  }
  it should "return None when missing" in {
    case class Person(name: String, age: Option[Int]) {}
    val args = (
      required[String]("--name") and
      optionalFree[Int]
    ).to[Person]
    args.parse("--name", "foo") should succeedWith(Person("foo", None))
  }
  
  "repeated free arg" should "succeed when not present" in {
    case class Person(name: String, occupations: List[String]) {}
    val args = (
      required[String]("--name") and
      repeatedFree[String]
    ).to[Person]
    args.parse("--name", "foo") should succeedWith(Person("foo", Nil))
  }
  it should "succeed when mixed around" in {
    case class Person(name: String, age: Int, occupations: List[String]) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age") and
      repeatedFree[String]
    ).to[Person]
    args.parse("x", "--name", "foo", "y", "--age", "50", "z") should succeedWith(Person("foo", 50, List("x","y","z")))
  }
  
  "repeatedAtLeastOnceFree arg" should "fail when not present" in {
    case class Person(name: String, occupations: List[String]) {}
    val args = (
      required[String]("--name") and
      repeatedAtLeastOnceFree[String]
    ).to[Person]
    args.parse("--name", "foo") should fail
  }
  it should "suceed when mixed around" in {
    case class Person(name: String, age: Int, occupations: List[String]) {}
    val args = (
      required[String]("--name") and
      required[Int]("--age") and
      repeatedAtLeastOnceFree[String]
    ).to[Person]
    args.parse("x", "--name", "foo", "y", "--age", "50", "z") should succeedWith(Person("foo", 50, List("x","y","z")))
  }
  
  "Implicit conversion with 'as'" should "succeed" in {
    case class Person(name: String) {}
    import scala.language.implicitConversions
    implicit def nameToPerson(name: String): Person = Person(name)
    val arg = required[String]("--name").as[Person]
    arg.parse("--name", "foo") should succeedWith(Person("foo"))
  }
  
  "Flag argument" should "be false when not present" in {
    val arg = flag("--foo")
    arg.parse() should succeedWith(false)
  }
  it should "be true when present" in {
    val arg = flag("--foo")
    arg.parse("--foo") should succeedWith(true)
  }
  
  "Arg with default" should "use default value when unspecified" in {
    val arg = optional[String]("--foo").default("bar")
    arg.parse() should succeedWith("bar")
  }
  it should "use value when specified" in {
    val arg = optional[String]("--foo").default("bar")
    arg.parse("--foo", "baz") should succeedWith("baz")
  }
  
  "Or arg" should "succeed left" in {
    val arg = required[Int]("--foo").or(required[Int]("--bar"), _ + _)
    arg.parse("--foo", "1") should succeedWith(1)
  }
  it should "succeed right" in {
    val arg = required[Int]("--foo").or(required[Int]("--bar"), _ + _)
    arg.parse("--bar", "2") should succeedWith(2)
  }
  it should "succeed and combine both" in {
    val arg = required[Int]("--foo").or(required[Int]("--bar"), _ + _)
    arg.parse("--foo", "1", "--bar", "2") should succeedWith(3)
  }
  it should "fail when missing both" in {
    val arg = required[Int]("--foo").or(required[Int]("--bar"), _ + _)
    arg.parse() should fail
  }
  
  "Xor arg" should "succeed left" in {
    val arg = required[Int]("--foo") xor required[Int]("--bar")
    arg.parse("--foo", "1") should succeedWith(1)
  }
  it should "succeed right" in {
    val arg = required[Int]("--foo") xor required[Int]("--bar")
    arg.parse("--bar", "2") should succeedWith(2)
  }
  it should "fail with both" in {
    val arg = required[Int]("--foo") xor required[Int]("--bar")
    arg.parse("--foo", "1", "--bar", "2") should fail
  }
  it should "succeed fail when missing both" in {
    val arg = required[Int]("--foo") xor required[Int]("--bar")
    arg.parse() should fail
  }
  
  "Constant arg" should "return its value" in {
    constant(10).parse() should succeedWith(10)
  }
  
  "String parser" should "work" in {
    required[String]("--foo").parse("--foo", "bar") should succeedWith("bar")
  }
  "Boolean parser" should "work with valid value" in {
    required[Boolean]("--foo").parse("--foo", "true") should succeedWith(true)
  }
  it should "not work with invalid value" in {
    required[Boolean]("--foo").parse("--foo", "asdf") should fail
  }
  "Byte parser" should "work with valid value" in {
    required[Byte]("--foo").parse("--foo", "1") should succeedWith(1.toByte)
  }
  it should "not work with invalid value" in {
    required[Byte]("--foo").parse("--foo", "asdf") should fail
  }
  "Short parser" should "work with valid value" in {
    required[Short]("--foo").parse("--foo", "1") should succeedWith(1.toShort)
  }
  it should "not work with invalid value" in {
    required[Short]("--foo").parse("--foo", "asdf") should fail
  }
  "Int parser" should "work with valid value" in {
    required[Int]("--foo").parse("--foo", "1") should succeedWith(1)
  }
  it should "not work with invalid value" in {
    required[Int]("--foo").parse("--foo", "asdf") should fail
  }
  "Long parser" should "work with valid value" in {
    required[Long]("--foo").parse("--foo", "1") should succeedWith(1l)
  }
  it should "not work with invalid value" in {
    required[Long]("--foo").parse("--foo", "asdf") should fail
  }
  "Float parser" should "work with valid value" in {
    required[Float]("--foo").parse("--foo", "1.2") should succeedWith(1.2f)
  }
  it should "not work with invalid value" in {
    required[Float]("--foo").parse("--foo", "asdf") should fail
  }
  "Double parser" should "work with valid value" in {
    required[Double]("--foo").parse("--foo", "1.2") should succeedWith(1.2)
  }
  it should "not work with invalid value" in {
    required[Double]("--foo").parse("--foo", "asdf") should fail
  }
  "Char parser" should "work with valid value" in {
    required[Char]("--foo").parse("--foo", "a") should succeedWith('a')
  }
  it should "not work with invalid value" in {
    required[Char]("--foo").parse("--foo", "asdf") should fail
  }
  
  "Either parser" should "work left" in {
    required[Either[Int, Char]]("--foo").parse("--foo", "1") should succeedWith(Left(1): Either[Int, Char])
  }
  it should "work right" in {
    required[Either[Int, Char]]("--foo").parse("--foo", "a") should succeedWith(Right('a'): Either[Int, Char])
  }
  it should "fail with invalid" in {
    required[Either[Int, Char]]("--foo").parse("--foo", "abc") should fail
  }
  it should "nest with list" in {
    val arg = required[Either[List[Int], Char]]("--foo")
    arg.parse("--foo", "1,2,3") should succeedWith(Left(List(1,2,3)): Either[List[Int], Char])
  }
  
  "List parser" should "work with empty list" in {
    required[List[Int]]("--foo").parse("--foo", "") should succeedWith(List.empty[Int])
  }
  it should "work with comma separated list" in {
    required[List[Int]]("--foo").parse("--foo", "1,2,3") should succeedWith(List(1,2,3))
  }
  it should "fail with invalid element" in {
    required[List[Int]]("--foo").parse("--foo", "1,a,3") should fail
  }
  it should "nest with either" in {
    val arg = required[List[Either[Int, Char]]]("--foo")
    arg.parse("--foo", "1,b,3") should succeedWith(List(Left(1),Right('b'),Left(3)))
  }
  
  "Parser" should "fail on unused argument" in {
    val arg = optionalFree[String]
    arg.parse("foo", "bar") should fail
  }
  it should "fail on failing free arg" in { // honestly, just for dat sweet sweet 100% coverage
    val arg = new Arg[Int] {
      def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode) = {
        if (free) VisitError("boo!") else VisitNoop
      }
      def complete = Success(0)
    }
    arg.parse("foo") should fail
  }
  it should "work on a list of args" in {
    required[String]("--foo").parse(List("--foo", "bar"), SpaceSeparated) should succeedWith("bar")
  }
  it should "work on an array of args" in {
    required[String]("--foo").parse(Array("--foo", "bar")) should succeedWith("bar")
  }
  
  def succeedWith[A](expected: A): Matcher[Try[A]] = new Matcher[Try[A]] {
    def apply(left: Try[A]) = left match {
      case Failure(e) => MatchResult(false, s"Failed with ${e.getMessage()}", s"Failed with ${e.getMessage()}")
      case Success(a) => MatchResult(a == expected, s"Succeeded but $a did not match $expected", s"Succeeded and $a matched $expected")
    }
  }
  
  def fail: Matcher[Try[Any]] = new Matcher[Try[Any]] {
    def apply(left: Try[Any]) = MatchResult(left.isFailure, "Expected failure, got success", "Expected success, got failure")
  }
  
}
