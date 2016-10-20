package jmms

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition, Position}
import JToken._


trait Ranged{
  var range: Option[(Int,Int)] = None
  def withRange(start: Int, end: Int): this.type = {
    if(range.isEmpty)
      range = Some((start, end))
    this
  }
}

sealed trait JToken extends Ranged{
  type V
  def data: V
  def getRange = range.get
}

object JToken{
  case class TOp(data: String) extends JToken {type V = String}
  case class TSep(data: String) extends JToken {type V = String}
  case class TReserve(data: JKeyword.Value) extends JToken {type V = JKeyword.Value}
  case class TIdentifier(data: String) extends JToken {type V = String}

  case class TInt(data: Int) extends JToken {type V = Int}
  case class TChar(data: String) extends JToken {type V = String}
  case class TString(data: String) extends JToken {type V = String}

}



/** turn a line of code into tokens */
object Tokenizer extends JavaTokenParsers {

  /** `ranged` decorates a parser's result with the start and end position of the
    *  input it consumed.
    *
    * @param p a `Parser` whose result conforms to `Ranged`.
    * @return A parser that has the same behaviour as `p`, but which marks its
    *         result with the start and end position of the input it consumed,
    *         if it didn't already have a position.
    */
  private def ranged[T <: Ranged](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success(t.withRange(in.pos.column, in1.pos.column), in1)
      case ns: NoSuccess => ns
    }
  }


  def pOp = ranged{ JOp.operatorRegex filter(d => !d.startsWith("//")) map TOp }

  def pComment: Parser[String] = """//.*""".r withFailureMessage "illegal token found" // Have to put error message here for it to work

  def pIdentifier = ranged( ident ^^ { l => JKeyword.string2keyword.get(l) match {
    case Some(kv) => TReserve(kv)
    case None => TIdentifier(l)
  }} )


  def pSep = ranged( """[,.{()};\[\]]""".r ^^ TSep)

//  private val ESC = """(\\[nrtbf’"\\])"""

  def pCharLiteral = ranged( """'((\\[nrtb'"\\])|[^'\\\n])'""".r ^^ {s => TChar(s.substring(1, s.length-1))} )

  def pStringLiteral = ranged(stringLiteral ^^ TString)

  def pIntLiteral = ranged( wholeNumber ^^ (s => TInt(s.toInt)))

  def pToken = pIdentifier | pSep | pOp | pIntLiteral | pStringLiteral | pCharLiteral

  def pLine = rep(pToken) <~ pComment.?


  def tokenizeALine(line: CharSequence, shift: Int): ParseResult[Stream[JToken]] = {
    val reader = new ShiftReader(line, shift)
    parseAll(pLine, reader).map(_.toStream)
  }


  def tokenizeSource(lines: Iterator[CharSequence]): Either[(String, Int), Iterable[JToken]] = {
    var result = ListBuffer[JToken]()
    var shift = 0
    lines.foreach(line => {
      val r = tokenizeALine(line, shift)
      shift += line.length()+1 // new line at the end also counts
      if(r.successful) result ++= r.get
      else return Left(r.toString, r.next.pos.column)
    })
    Right(result)
  }
}
