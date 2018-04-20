name := "s-99-solutions"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

testOptions in Test += Tests.Argument("-oD")