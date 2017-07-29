package com.joefkelley.argyle

sealed trait ArgMode
object SpaceSeparated extends ArgMode
object EqualsSeparated extends ArgMode