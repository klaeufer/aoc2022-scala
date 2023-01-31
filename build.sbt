name := "aoc2022"

version := "0.1"

scalaVersion := "3.2.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Yexplicit-nulls", "-Ysafe-init", "-language:strictEquality")

coverageEnabled := true

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "2.1.0",
  "org.scalatest" %% "scalatest"  % "3.2.15"  % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test"
)

logBuffered := false

Test / parallelExecution := false

enablePlugins(JavaAppPackaging)

