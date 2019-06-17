name := "baudrillard"
organization := "com.github.jeremyrsmith"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:experimental.macros"
)

//initialCommands in console := "import baudrillard.symbolic._\n\n"
