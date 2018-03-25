
name := "JSAnimation"
version := "0.1-SNAPSHOT"
scalaVersion := "2.12.2"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  "com.lihaoyi" %%% "utest" % "0.4.6" % "test"
)

scalaJSUseMainModuleInitializer := true

// needed for tests
jsDependencies += RuntimeDOM
testFrameworks += new TestFramework("utest.runner.Framework")

