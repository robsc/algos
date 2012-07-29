organization := "com.rschonberger"

name := "algorithms"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.9.1"

// seq(webSettings :_*)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "org.scala-tools.time" %% "time" % "0.5"
)

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
