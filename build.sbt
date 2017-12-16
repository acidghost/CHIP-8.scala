name := "CHIP-8.scala"
description := "A simple CHIP-8 emulator in Scala (using functional programming patterns)"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-RC1"
)

fork in run := true
