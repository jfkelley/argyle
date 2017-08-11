![Argyle](argyle-logo-msmall.png)

Command-line argument parsing for Scala

## Example:

```scala
import com.joefkelley.argyle._
import scala.util.{Try, Success, Failure}

object Example extends App {

  val a = (
    required[String]("--name", "-n") and
    optional[String]("--occupation") and
    required[Int]("--age") and
    repeated[String]("--hobby") and
    requiredOneOf("--single" -> Single, "--married" -> Married) and
    optional[Int]("--fingers").default(10) and
    flag("--debug") and
    optionalBranch[Pet](
      "--pet-rock" -> required[String]("--rock-name").to[Rock],
      "--pet-dog"  -> (required[String]("--dog-breed") and required[String]("--dog-name")).to[Dog]
    ) and
    (requiredOneOf("--male" -> Male, "--female" -> Female) xor required[String]("--gender").to[OtherGender])
  ).to[PersonConfig]

  a.parse(args) match {
    case Success(person) => println(person)
    case Failure(e) => throw e
  }

  case class PersonConfig(
      name: String,
      occupation: Option[String],
      age: Int,
      hobbies: List[String],
      maritalStatus: MaritalStatus,
      nFingers: Int,
      likesToDebug: Boolean,
      pet: Option[Pet],
      gender: Gender)

  sealed trait MaritalStatus
  object Single extends MaritalStatus
  object Married extends MaritalStatus

  trait Pet
  case class Dog(breed: String, name: String) extends Pet
  case class Rock(name: String) extends Pet

  sealed trait Gender
  object Male extends Gender
  object Female extends Gender
  case class OtherGender(str: String) extends Gender

}
```

## Background
Argyle is a simple tool for parsing command-line arguments. Its goals are:

- Absolute minimum boilerplate
- Type-safe
- Flexible / Extensible
- Minimal dependencies
- Pure functional

It does not currently have the ability to print help/usage information. The hope is that the syntax is so concise and readable that the code is documentation enough. Including this feature would be a crutch that would allow compromising on the first goal ;)

## Usage
Three steps:

1. Define a case class containing all of the options you want to configure
2. Construct a `com.joefkelley.argyle.Arg[<config class>]` by "and"-ing together more granular Args representing individual fields
3. Call `.parse(args)` on that, returning a `Try[<config class>]`

Here's a simple example that requires two command-line arguments: "--name" and "--age":

```scala
case class Person(name: String, age: Int)
val nameArg = required[String]("--name")
val ageArg = required[Int]("--age")
val personArg = (nameArg and ageArg).to[Person]
val result: Try[Person] = personArg.parse(args)
```

Breaking this down line-by-line:

```scala
case class Person(name: String, age: Int)
```

This class will eventually contain all of the configuration options we want from the command line

```scala
val nameArg = required[String]("--name")
```

Creates an `Arg[String]` that will match arguments passed in the form "--name Joe" or similar. Note that because the arg is required, parsing will fail if it is not present.

```scala
val ageArg = required[Int]("--age")
```

The specified argument type will be the type of the resulting object that is eventually returned by the parser. In this case it is Int, so this creates an `Arg[Int]`. The signature of the `required` method is:

```scala
def required[A : Reader](keys: String*): Arg[A]
```

Note that the type requires an implicit typeclass `com.joefkelley.argyle.Reader[A]`. There are built-in values for all primitive types, Strings, Lists (comma-separated), and Eithers. If you wish to use some different parsing logic, the Reader trait has a very simple interface that you can implement.

```scala
val personArg = (nameArg and ageArg).to[Person]
```

The `and` method here combines an `Arg[String]` and an `Arg[Int]` into an `Arg[String :: Int :: HNil]`, an arg parser for a shapeless hlist. But there is no need to deal with shapeless directly. The `.to[Person]` call converts this to an `Arg[Person]`. Note that this is still type-safe; the types must match the fields of the `Person` class or it would not compile.

```scala
val result: Try[Person] = personArg.parse(args)
```

Finally, we call `.parse(args)`, which will return a `Success[Person]` iff both "--name" and "--age" are supplied, the argument for "--age" is an integer, and there are no other unused arguments. Otherwise, it will return a `Failure` containing an appropriate error message. By default, the args are expected in the form "--name Joe --age 100", but equals-separated format can also be used by calling `.parse(args, com.joefkelley.argyle.EqualsSeparated)`, for example "--name=Joe --age=100".

## Full API Details

**The `com.joefkelley.argyle` package object contains several methods for creating `Arg`s. They are:**

```scala
def required[A : Reader](keys: String*): Arg[A]
```
Requires that one of the given keys must be present exactly once, and will fail otherwise.

```scala
def optional[A : Reader](keys: String*): Arg[Option[A]]
```
Requires that one of the given keys will be present at most once. Will result in a `None` if not present, a `Some[A]` if present, and fail if present more than once.

```scala
def repeated[A : Reader](keys: String*): Arg[List[A]]
```

Matches any one of the keys any number of times, resulting in a `List[A]`. For example, `repeated[Int]("-n")` would successfully parse "-n 1 -n 5 -n 10", resulting in `List(1, 5, 10)`.

```scala
def repeatedAtLeastOnce[A : Reader](keys: String*): Arg[List[A]]
```
Same as above, but fails if not present at least once. Always results in a list with at least one element.

```scala
def requiredOneOf[A](kvs: (String, A)*): Arg[A]
```
A parser that matches exactly one of the keys in the key-value pairs, resulting in its corresponding value. Note that no value should be passed in the command-line arguments. For example, `requiredOneOf("-a" -> 1, "-b" -> 2)` would match just "-a", resulting in a value of 1. Fails if none of the keys are present.

```scala
def optionalOneOf[A](kvs: (String, A)*): Arg[Option[A]]
```
Same as above, except returns a `None` instead of failing if none are present, and returns a `Some[A]` if one is.

```scala
def flag(keys: String*): Arg[Boolean]
```
If any of the provided keys are present, results in `true`, otherwise `false`.

```scala
def requiredFree[A : Reader]: Arg[A]
```
Matches any argument, with lower priority than "keyed" arguments. For example, `requiredFree[String] and required[Int]("--n")` would match "--n 5 foo". Fails if no extra args are present.

```scala
def optionalFree[A : Reader]: Arg[Option[A]]
```
Same as above, except returns a `None` instead of failing if none are present, and returns a `Some[A]` if one is.

```scala
def repeatedFree[A : Reader]: Arg[List[A]]
```
Matches any number of arguments not matched by "keyed" arguments.

```scala
def repeatedAtLeastOnceFree[A : Reader]: Arg[List[A]]
```
Same as above, except fails if no arguments are present.

Note that the order of free arguments is important, and no special "back-tracking" is done; they are matched greedily. For example, `repeatedFree[String] and requiredFree[String]` will always fail because `repeatedFree[String]` will consume all unmatched arguments since it is first in the `and`.

Also note that free args do not work correctly when combined using `or` or `xor`. To do so correctly would require back-tracking that is not currently implemented.

```scala
def requiredBranch[A](kvs: (String, Arg[A])*): Arg[A]
```
Allows branching behavior based on the presence of keys in the key-value pairs. "Activates" one of the passed args based on which key is present, and parses using that arg. For example: `requiredBranch("-a" -> required[String]("--foo"), "-b" -> requiredFree[String])` would parse either "-a --foo hello" or "-b hello". Arbitrarily-complex args can be used, including nested branches or anything else. Note that the "branching" argument must be present in the command line arguments before the arguments parsed within that branch (i.e. "--foo hello -a" would not work).

```scala
def optionalBranch[A](kvs: (String, Arg[A])*): Arg[Option[A]]
```
Same as above, but does not fail if none of the branching keys are present.

```scala
def constant[A](a: A): Arg[A]
```
Does not consume or require any command-line arguments, just returns the value back. Useful for configuration options that can't be changed, or for simple cases of branching arguments.

**The `com.joefkelley.argyle.Arg[A]` class itself contains methods for combining and modifying args. They are:**

```scala
def flatMap[B](f: A => Try[B]): Arg[B]
```
If parsing succeeds, applies `f` to the result, and succeeds if the result is a success, otherwise fails.

```scala
def map[B](f: A => B): Arg[B] = flatMap(a => Success(f(a)))
```
If parsing succeeds, applies `f` to the result and returns the output.

```scala
def as[B](implicit f: A => B): Arg[B] = map(f)
```
Syntactic sugar for `map` for cases when an implicit conversion `A => B` is possible.

```scala
def xor[B >: A](arg2: Arg[B]): Arg[B]
```
Returns a new Arg that succeeds if either this, or the passed in arg is present and succeeds. Notably fails if they are both present. For example, `required[Int]("-n").xor(required[Int]("-m"))` would return 1 for both "-n 1" and "-m 1", but would fail for "-n 1 -m 1".

```scala
def or[B >: A](arg2: Arg[B], f: (B, B) => B): Arg[B]
```
Same as `xor`, except does not fail if both are present. Instead, calls `f` with the output from both. (In the order `f(thisOutput, arg2Output)`).

Also note that free args do not work correctly when combined using `or` or `xor`. To do so correctly would require back-tracking that is not currently implemented.

```scala
implicit class Default[A](arg: Arg[Option[A]]) {
  def default(a: A): Arg[A]
}
```
For arguments that result in an `Option[A]`, such as those from `optional[A](...)`, provides an extension `default` method that promotes it from an `Arg[Option[A]]` to an `Arg[A]` by providing a default result value for when the argument is not present.

```scala
and
```
The signature for the `and` method is a bit more complex because it involves a small amount of shapeless magic. If `a` is an `Arg[A]`, and `b` is an `Arg[B]`, and `A` is not an `HList`, then `a and b` returns an `Arg[A :: B :: HNil]`. However, if `A` *is* an `HList`, then the resulting `Arg` has `B` appended to the end of `A`. If this was not the case, then `a and b and c` would return an `Arg[(A :: B :: HNil) :: C :: HNil]`, which is subtly different than the expected `Arg[A :: B :: C :: HNil]` that is actually returned.

```scala
to
```
The `arg.to[MyClass]` is useful to avoid dealing with shapeless `HLists` directly. Can be used if the type of `arg` is an `HList` and the output class's fields match exactly the types of the `HList`.

**The `Arg` class is meant to be extensible**

Custom parsing logic can added by implementing the `Arg` trait's two abstract methods:

```scala
sealed abstract class VisitResult[+A]
case class VisitError(msg: String) extends VisitResult[Nothing]
case class VisitConsume[+A](next: Arg[A], remaining: List[String]) extends VisitResult[A]
object VisitNoop extends VisitResult[Nothing]

trait Arg[+A] {

  def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[A]

  def complete: Try[A]

}
```

The `visit` method will be called multiple times, passing in smaller and smaller subsets of the command-line arguments. If the head of the passed list is not relevant to the Arg, it should return `VisitNoop`. If the head is relevant, but incorrect or unexpected in some way, it should return `VisitError`. Any errors will fail the entire parse. If the head is relevant and correct, A `VisitConsume` should be returned containing a new `Arg` to use for parsing instead of this from here on out, and the remaining unmatched/unconsumed part of the arguments. If you do not wish to use a purely-functional style, you may instead mutate the `Arg` and return `this`.

The branching arg class is a good simple example implementation:
```scala
class BranchArg[A](branch: Map[String, Arg[A]]) extends Arg[Option[A]] {
  override def visit(xs: NonEmptyList[String], free: Boolean, mode: ArgMode): VisitResult[Option[A]] = xs match {
    case NonEmptyList(x, rest) => branch.get(x) match {
      case Some(arg) => VisitConsume(arg.map(a => Some(a)), rest)
      case None => VisitNoop
    }
  }
  override def complete: Try[Option[A]] = Success(None)
}
```

**An instance of the `Reader` typeclass is required for parsing individual argument values from strings to specific types**

This is the one section of code that is somewhat less type-safe (it's "stringly-typed"), so it is isolated and very simple. Default instances are provided for: String, Boolean, Byte, Short, Int, Long, Float, Double, Char, `List[A : Reader]` (comma-separated), and `Either[A : Reader, B : Reader]`.

If you wish to parse to some other class, this is also extensible. The trait to implement is:
```scala
trait Reader[A] {
  def apply(s: String): Try[A]
}
```

Built-in instances are, unsurprisingly, very simple examples to follow:
```scala
implicit val IntParser = new Reader[Int] { def apply(s: String) = Try(s.toInt) }
```
