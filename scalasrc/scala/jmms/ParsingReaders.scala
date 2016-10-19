package jmms

import scala.util.parsing.input.{CharSequenceReader, Position}

//class LineReader(src: CharSequence, lineNumber: Int, offset: Int = 0) extends CharSequenceReader(src, offset){
//  override def pos: Position = new Position {
//    override def column: Int = offset
//
//    override def line: Int = lineNumber
//
//    override protected def lineContents: String = src.toString
//
//  }
//
//  override def drop(n: Int): CharSequenceReader = new LineReader(src, lineNumber, offset+n)
//}

class ShiftReader(src: CharSequence, shift: Int, offset: Int = 0) extends CharSequenceReader(src, offset) {
  override def pos: Position = new Position {
    override def column: Int = offset + shift

    override def line: Int = 1

    override protected def lineContents: String = src.toString

    override def longString: String = {
      lineContents+"\n"+lineContents.take(offset-1).map{x => if (x == '\t') x else ' ' } + "^"
    }
  }

  override def rest: CharSequenceReader =
    if (offset < source.length) new ShiftReader(source, shift, offset + 1)
    else this

  override def drop(n: Int): CharSequenceReader = new ShiftReader(src, shift, offset+n)
}