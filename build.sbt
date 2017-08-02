name := "gofly"

version := "1.0"

scalaVersion := "2.12.1"


libraryDependencies ++= Seq(
  "com.propensive" %% "rapture" % "2.0.0-M8",
  "org.jsoup" % "jsoup" % "1.10.1",
  "codes.reactive" %% "scala-time" % "0.4.1",
  "com.google.guava" % "guava" % "21.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)