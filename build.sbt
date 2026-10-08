name := "StarCrafter"

version := "1.0"

scalaVersion := "3.10.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "5.9.1" % Test,
  "org.tinylog" % "tinylog-api" % "2.8.1",
  "org.tinylog" % "tinylog-impl" % "2.8.1",
  "commons-io" % "commons-io" % "2.22.0"
)

scalacOptions ++= Seq("-deprecation", "-feature", "-release", "17")
javacOptions ++= Seq("--release", "17")
