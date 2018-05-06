package com.joefkelley.argyle

import org.scalatest._
import org.scalatest.matchers._
import com.joefkelley.argyle._
import scala.util.{Failure, Success, Try}
import java.io.File
import java.nio.file.Paths
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import java.time.LocalDate
import java.time.LocalTime
import java.time.LocalDateTime

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
  
  "Backtracking" should "work with multiple free args" in {
    case class A1(x: Int, y: Option[String])
    val a1 = (requiredFree[Int] and optionalFree[String]).to[A1]
    a1.parse("1", "foo") should succeedWith(A1(1, Some("foo")))
    a1.parse("1") should succeedWith(A1(1, None))
    
    case class A2(x: Option[Int], y: String)
    val a2 = (optionalFree[Int] and requiredFree[String]).to[A2]
    a2.parse("1", "foo") should succeedWith(A2(Some(1), "foo"))
    a2.parse("foo") should succeedWith(A2(None, "foo"))
    
    case class A3(x: List[Int], y: String)
    val a3 = (repeatedFree[Int] and requiredFree[String]).to[A3]
    a3.parse("1", "2", "foo") should succeedWith(A3(List(1, 2), "foo"))
    a3.parse("1", "foo", "2") should succeedWith(A3(List(1, 2), "foo"))
    a3.parse("foo", "1", "2") should succeedWith(A3(List(1, 2), "foo"))
  }
  it should "work with free args in ors" in {
    case class Person(name: String, age: Int)
    val a = ((requiredFree[String] xor required[String]("--s")) and requiredFree[Int]).to[Person]
    a.parse("1", "--s", "foo") should succeedWith(Person("foo", 1))
    a.parse("1", "foo") should succeedWith(Person("foo", 1))
    a.parse("foo", "1") should succeedWith(Person("foo", 1))
  }
  it should "not stackoverflow with 100 args" in {
    case class A1(
      n1: Int, n2: Int, n3: Int, n4: Int, n5: Int,
      n6: Int, n7: Int, n8: Int, n9: Int, n10: Int
    )
    case class A2(
      n11: Int, n12: Int, n13: Int, n14: Int, n15: Int,
      n16: Int, n17: Int, n18: Int, n19: Int, n20: Int
    )
    case class A3(
      n21: Int, n22: Int, n23: Int, n24: Int, n25: Int,
      n26: Int, n27: Int, n28: Int, n29: Int, n30: Int
    )
    case class A4(
      n31: Int, n32: Int, n33: Int, n34: Int, n35: Int,
      n36: Int, n37: Int, n38: Int, n39: Int, n40: Int
    )
    case class A5(
      n41: Int, n42: Int, n43: Int, n44: Int, n45: Int,
      n46: Int, n47: Int, n48: Int, n49: Int, n50: Int
    )
    case class A6(
      n51: Int, n52: Int, n53: Int, n54: Int, n55: Int,
      n56: Int, n57: Int, n58: Int, n59: Int, n60: Int
    )
    case class A7(
      n61: Int, n62: Int, n63: Int, n64: Int, n65: Int,
      n66: Int, n67: Int, n68: Int, n69: Int, n70: Int
    )
    case class A8(
      n71: Int, n72: Int, n73: Int, n74: Int, n75: Int,
      n76: Int, n77: Int, n78: Int, n79: Int, n80: Int
    )
    case class A9(
      n81: Int, n82: Int, n83: Int, n84: Int, n85: Int,
      n86: Int, n87: Int, n88: Int, n89: Int, n90: Int
    )
    case class A10(
      n91: Int, n92: Int, n93: Int, n94: Int, n95: Int,
      n96: Int, n97: Int, n98: Int, n99: Int, n100: Int
    )
    case class B(
      a1: A1, a2: A2, a3: A3, a4: A4, a5: A5,
      a6: A6, a7: A7, a8: A8, a9: A9, a10: A10
    )
    val a1 = (
      required[Int]("n1") and required[Int]("n2") and
      required[Int]("n3") and required[Int]("n4") and
      required[Int]("n5") and required[Int]("n6") and
      required[Int]("n7") and required[Int]("n8") and
      required[Int]("n9") and required[Int]("n10")
    ).to[A1]
    val a2 = (
      required[Int]("n11") and required[Int]("n12") and
      required[Int]("n13") and required[Int]("n14") and
      required[Int]("n15") and required[Int]("n16") and
      required[Int]("n17") and required[Int]("n18") and
      required[Int]("n19") and required[Int]("n20")
    ).to[A2]
    val a3 = (
      required[Int]("n21") and required[Int]("n22") and
      required[Int]("n23") and required[Int]("n24") and
      required[Int]("n25") and required[Int]("n26") and
      required[Int]("n27") and required[Int]("n28") and
      required[Int]("n29") and required[Int]("n30")
    ).to[A3]
    val a4 = (
      required[Int]("n31") and required[Int]("n32") and
      required[Int]("n33") and required[Int]("n34") and
      required[Int]("n35") and required[Int]("n36") and
      required[Int]("n37") and required[Int]("n38") and
      required[Int]("n39") and required[Int]("n40")
    ).to[A4]
    val a5 = (
      required[Int]("n41") and required[Int]("n42") and
      required[Int]("n43") and required[Int]("n44") and
      required[Int]("n45") and required[Int]("n46") and
      required[Int]("n47") and required[Int]("n48") and
      required[Int]("n49") and required[Int]("n50")
    ).to[A5]
    val a6 = (
      required[Int]("n51") and required[Int]("n52") and
      required[Int]("n53") and required[Int]("n54") and
      required[Int]("n55") and required[Int]("n56") and
      required[Int]("n57") and required[Int]("n58") and
      required[Int]("n59") and required[Int]("n60")
    ).to[A6]
    val a7 = (
      required[Int]("n61") and required[Int]("n62") and
      required[Int]("n63") and required[Int]("n64") and
      required[Int]("n65") and required[Int]("n66") and
      required[Int]("n67") and required[Int]("n68") and
      required[Int]("n69") and required[Int]("n70")
    ).to[A7]
    val a8 = (
      required[Int]("n71") and required[Int]("n72") and
      required[Int]("n73") and required[Int]("n74") and
      required[Int]("n75") and required[Int]("n76") and
      required[Int]("n77") and required[Int]("n78") and
      required[Int]("n79") and required[Int]("n80")
    ).to[A8]
    val a9 = (
      required[Int]("n81") and required[Int]("n82") and
      required[Int]("n83") and required[Int]("n84") and
      required[Int]("n85") and required[Int]("n86") and
      required[Int]("n87") and required[Int]("n88") and
      required[Int]("n89") and required[Int]("n90")
    ).to[A9]
    val a10 = (
      required[Int]("n91") and required[Int]("n92") and
      required[Int]("n93") and required[Int]("n94") and
      required[Int]("n95") and required[Int]("n96") and
      required[Int]("n97") and required[Int]("n98") and
      required[Int]("n99") and required[Int]("n100")
    ).to[A10]
    val b = (
      a1 and a2 and a3 and a4 and a5 and
      a6 and a7 and a8 and a9 and a10
    ).to[B]
    
    val expected = B(
      A1(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      A2(11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
      A3(21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
      A4(31, 32, 33, 34, 35, 36, 37, 38, 39, 40),
      A5(41, 42, 43, 44, 45, 46, 47, 48, 49, 50),
      A6(51, 52, 53, 54, 55, 56, 57, 58, 59, 60),
      A7(61, 62, 63, 64, 65, 66, 67, 68, 69, 70),
      A8(71, 72, 73, 74, 75, 76, 77, 78, 79, 80),
      A9(81, 82, 83, 84, 85, 86, 87, 88, 89, 90),
      A10(91, 92, 93, 94, 95, 96, 97, 98, 99, 100)
    )
    
    val input = (1 to 100).toArray.flatMap { i => Array("n" + i, i.toString) }
    b.parse(input) should succeedWith(expected)
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
  "File parser" should "work with valid value" in {
    required[File]("--foo").parse("--foo", "/foo") should succeedWith(new File("/foo"))
  }
  "Path parser" should "work with valid value" in {
    required[Path]("--foo").parse("--foo", "/foo") should succeedWith(Paths.get("/foo"))
  }
  "Scala Duration parser" should "work with valid value" in {
    import scala.concurrent.duration.Duration
    required[Duration]("--foo").parse("--foo", "1s") should succeedWith[Duration](Duration(1, TimeUnit.SECONDS))
  }
  it should "not work with invalid value" in {
    import scala.concurrent.duration.Duration
    required[Duration]("--foo").parse("--foo", "asdf") should fail
  }
  "Java Duration parser" should "work with valid value" in {
    import java.time.Duration
    required[Duration]("--foo").parse("--foo", "PT1S") should succeedWith(Duration.ofSeconds(1))
  }
  it should "not work with invalid value" in {
    import java.time.Duration
    required[Duration]("--foo").parse("--foo", "asdf") should fail
  }
  "Date parser" should "work with valid value" in {
    required[LocalDate]("--foo").parse("--foo", "1970-01-01") should succeedWith(LocalDate.of(1970, 1, 1))
  }
  it should "not work with invalid value" in {
    required[LocalDate]("--foo").parse("--foo", "asdf") should fail
  }
  "Time parser" should "work with valid value" in {
    required[LocalTime]("--foo").parse("--foo", "00:00:00") should succeedWith(LocalTime.of(0, 0, 0))
  }
  it should "not work with invalid value" in {
    required[LocalTime]("--foo").parse("--foo", "asdf") should fail
  }
  "DateTime parser" should "work with valid value" in {
    required[LocalDateTime]("--foo").parse("--foo", "1970-01-01T00:00:00") should succeedWith(LocalDateTime.of(1970, 1, 1, 0, 0, 0))
  }
  it should "not work with invalid value" in {
    required[LocalDateTime]("--foo").parse("--foo", "asdf") should fail
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
    arg.parse("--foo", "1,b,3") should succeedWith[List[Either[Int, Char]]](List(Left(1),Right('b'),Left(3)))
  }
  
  "Parser" should "fail on unused argument" in {
    val arg = optionalFree[String]
    arg.parse("foo", "bar") should fail
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
