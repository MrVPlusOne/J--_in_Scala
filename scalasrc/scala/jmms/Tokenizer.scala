package jmms

import scala.util.parsing.combinator.JavaTokenParsers
import JToken._

import scala.util.parsing.input.{CharSequenceReader, NoPosition, Position}


trait Ranged{
  var range: Option[(Int,Int)] = None
  var pos: Position = NoPosition
  def withRange(start: Int, end: Int, pos: Position): this.type = {
    if(range.isEmpty){
      range = Some((start, end))
      this.pos = pos
    }
    this
  }

  def withRangeOf(ranged: Ranged): this.type = {
    val (start, until) = ranged.getRange
    withRange(start, until, ranged.pos)
  }

  def getRange = range.getOrElse(throw new Exception(s"getRange on unset ranged: ${toString}"))

  def rangeSize = {
    val (start, end) = getRange
    end - start
  }
}

sealed trait JToken extends SyntaxTree{
  type V
  def data: V

  override def children: IndexedSeq[SyntaxTree] = IndexedSeq()

  override def nodeName: String = toString
}

object JToken{
  case class TOp(data: String) extends JToken {type V = String}
  case class TSep(data: String) extends JToken {
    type V = String
  }
  case class TReserve(data: JKeyword.Value) extends JToken {type V = JKeyword.Value}
  case class TIdentifier(data: String) extends JToken {type V = String}

  case class TInt(data: Int) extends JToken {type V = Int}
  case class TChar(data: String) extends JToken {type V = String}
  case class TString(data: String) extends JToken {type V = String}
  case class EndOfTokens(srcEnd: Int) extends JToken {
    type V = Unit
    def data = Unit
    range = Some((srcEnd, srcEnd))
  }

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
      case Success(t, in1) => Success(t.withRange(in.offset, in1.offset, in.pos), in1)
      case ns: NoSuccess => ns
    }
  }


  def pOp = ranged{ JOp.operatorRegex filter(d => !d.startsWith("//") && !d.startsWith("/*")) map TOp }

  def pSingleComment: Parser[String] = """//[^\n]*""".r

  def blockBody(in: Input): ParseResult[String] = {
    val start = in.offset
    val len = in.source.length()
    var i = start
    while (i < len - 1) {
      if (in.source.charAt(i) == '*' && in.source.charAt(i + 1) == '/')
        return Success(in.source.subSequence(in.offset, i).toString, in.drop(i - start + 2))
      i += 1
    }
    Failure("Block comment is not closed", in)
  }

  def pBlockComment: Parser[String] = "/*" ~! Parser[String](blockBody) ^^ {case _ ~ c => c}

  def pIdentifier = ranged( ident ^^ { l => JKeyword.string2keyword.get(l) match {
    case Some(kv) => TReserve(kv)
    case None => TIdentifier(l)
  }} )


  def pSep = ranged( """[,.{()};\[\]]""".r ^^ TSep)

//  private val ESC = """(\\[nrtbf’"\\])"""

  def pCharLiteral = ranged( """'((\\[nrtb'"\\])|[^'\\\n])'""".r ^^
    {s => TChar(s.substring(1, s.length-1))} ) withFailureMessage  "Char literal expected"

  def pStringLiteral = ranged(stringLiteral ^^ {s => TString(s.stripPrefix("\"").stripSuffix("\"")) })

  def pIntLiteral = ranged( wholeNumber ^^ (s => TInt(s.toInt)))

  def pToken = pIdentifier | pSep | pOp | pIntLiteral | pStringLiteral | pCharLiteral | failure("token expected")

  def pLine = rep(pToken) <~ pSingleComment.?

  def pSource = rep(pToken | pBlockComment | pSingleComment) map (r => {
    r.collect{ case tk: JToken => tk }
  })

  def tokenizeSource(source: CharSequence): Either[(String, Int), Iterable[JToken]] = {
    val reader = new CharSequenceReader(source, 0)
    val r = parseAll(pSource, reader)
    if(r.successful) Right(r.get)
    else Left(r.toString, r.next.offset)
  }

}
