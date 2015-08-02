name := "StarCrafter"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.4" % "test")
libraryDependencies += "org.tinylog" % "slf4j-binding" % "1.0"

scalacOptions += "-Xexperimental"