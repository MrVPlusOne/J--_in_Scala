package jmms

import scala.io.Source

/**
  * Created by weijiayi on 19/10/2016.
  */
object Test {
  def main(args: Array[String]): Unit = {
    val src = "hello"

    SyntaxParser.parseSource(SyntaxParser.pIdent, "hello")

  }
}
