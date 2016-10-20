package jmms

import scala.io.Source

/**
  * Created by weijiayi on 19/10/2016.
  */
object Test {
  def main(args: Array[String]): Unit = {
    val r = Tokenizer.tokenizeSource(Source.fromFile("tests/pass/Animalia.java").mkString)
    println(r)

  }
}
