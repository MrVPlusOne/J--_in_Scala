name := "jmm"

version := "1.0"

scalaVersion := "2.11.8"

javaSource in Compile := baseDirectory.value / "src"

scalaSource in Compile := baseDirectory.value / "scalasrc" / "scala"
scalaSource in Test := baseDirectory.value / "scalatest" / "scala"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "com.lihaoyi" %% "scalarx" % "0.3.1"
//  "net.bytebuddy" % "byte-buddy" % "1.4.31"
)
    