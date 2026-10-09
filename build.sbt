name := "StarCrafter"

version := "1.0"

scalaVersion := "3.10.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core"  % "5.9.1" % Test,
  "org.tinylog" % "tinylog-api"  % "2.8.1",
  "org.tinylog" % "tinylog-impl" % "2.8.1",
  "commons-io"  % "commons-io"   % "2.22.0"
)

// Every compiler warning fails the build.
scalacOptions ++= Seq("-deprecation", "-feature", "-Werror", "-release", "17")
javacOptions ++= Seq("--release", "17", "-Xlint:all", "-Werror")

Compile / mainClass := Some("pony.Controller")

// Formatting check plus the full, non-incremental test run; use before every push.
addCommandAlias("validate", ";scalafmtCheckAll;scalafmtSbtCheck;testFull")
