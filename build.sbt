name := "worldbrain"

version := "0.2"

scalaVersion := "2.12.1"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "1.8"

encoding := "UTF-8"

//***** Options & Dependencies *****
javacOptions ++= Seq(
  "-source", javaVersion.value,
  "-target", javaVersion.value,
  "-encoding", encoding.value
)

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-encoding", encoding.value
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.12" % "2.4.16",
  "org.spire-math" % "spire_2.12" % "0.13.0",
  "org.waman" % "scalatest-util" % "0.8" % "test",
  "org.scalafx" % "scalafx_2.12" % "8.0.102-R11" % "test"
)

fork in Global := true

crossPaths := false

    