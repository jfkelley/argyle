package com.joefkelley.argyle

import scala.util.{Try, Success, Failure}

object Example extends App {
  
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
  
  val a = (
    required[String]("--name") and
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
  
  
  
  
}