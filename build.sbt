import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.joefkelley",
      scalaVersion := "2.12.1",
      version      := "0.1.0"
    )),
    name := "argyle",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      shapeless
    ),

    // Sonatype settings
    homepage := Some(url("https://github.com/jfkelley/argyle")),
    scmInfo := Some(ScmInfo(url("https://github.com/jfkelley/argyle"),
                                "git@github.com:jfkelley/argyle.git")),
    developers += Developer("joefkelley",
                            "Joseph Kelley",
                            "joe@joefkelley.com",
                            url("http://www.joefkelley.com")),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    pomIncludeRepository := (_ => false)
  )
