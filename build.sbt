organization := "com.github.aerskine"

name := "generators"

version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.9.3", "2.10.0")

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0" withSources(),
  "org.scalaz" %% "scalaz-effect" % "7.0.0" withSources()
)
