package jmms

import scala.io.Source

/**
  * Created by weijiayi on 27/10/2016.
  */
object ParseTestFiles {
  import SemanticsAnalysis.fullyAnalyze

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("./src/example/Fib.java").mkString

    val pkg = fullyAnalyze(src, "fib example")
    val cf = pkg.classes.head.genClassFile("example/Fib", shouldLog = true)
    cf.writeToFile("./compiled/example/Fib.class")
  }
}
