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
}

object JToken{
  case class TOp(data: String) extends JToken {type V = String}
  case class TSep(data: String) extends JToken {type V = String}
  case class TReserve(data: JKeyword.Value) extends JToken {type V = JKeyword.Value}
  case class TIdentifier(data: String) extends JToken {type V = String}

  case class TInt(data: Int) extends JToken {type V = Int}
  case class TChar(data: String) extends JToken {type V = String}
  case class TString(data: String) extends JToken {type V = String}

  case class PositionedToken(token: JToken, start: Int, until: Int)
}



/** turn a line of code into tokens */
object Tokenizer extends JavaTokenParsers {
  def tokenConstruct[D](constructor: D => JToken) = (p: Position~D~Position) => {
    p match {
      case pos1~data~pos2 => PositionedToken(constructor(data), pos1.column, pos2.column)
    }
  }
  val tOp = tokenConstruct(TOp)
  val tReserve = tokenConstruct(TReserve)
  val tIdentifier = tokenConstruct(TIdentifier)
  val tSep = tokenConstruct(TSep)
  val tChar = tokenConstruct(TChar)
  val tString = tokenConstruct(TString)
  val tInt = tokenConstruct(TInt)

  /** `ranged` decorates a parser's result with the start and end position of the
    *  input it consumed.
    *
    * @param p a `Parser` whose result conforms to `Ranged`.
    * @return A parser that has the same behaviour as `p`, but which marks its
    *         result with the start and end position of the input it consumed,
    *         if it didn't already have a position.
    */
  def ranged[T <: Ranged](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success(t.withRange(in.pos.column, in1.pos.column), in1)
      case ns: NoSuccess => ns
    }
  }

  def withPos = new Parser[Position]{
    override def apply(in: Input): ParseResult[Position] = success(in.pos)(in)
  }

  def pOp = ranged{ JOp.operatorRegex filter(d => !d.startsWith("//")) map TOp }

//  def pOp = JOp.operatorRegex flatMap {
//    data=>
//      if (data.startsWith("//")) failure("operator can't start with //")
//      else success(TOp(data))
//  }

  def pComment: Parser[String] = """//.*""".r

  def pIdentifier = withPos ~ ident ~ withPos ^^ { case data @ p1 ~ l ~ p2 =>
    JKeyword.string2keyword.get(l) match {
      case Some(kv) => tReserve(this.~(this.~(p1, kv), p2))
      case None => tIdentifier(data)
    }
  }

  def pSep = withPos ~ """[,.{()};\[\]]""".r ~ withPos ^^ tSep

//  private val ESC = """(\\[nrtbf’"\\])"""

  def pCharLiteral = withPos ~ ("""'((\\[nrtb'"\\])|[^'\\\n])'""".r ^^ {s => s.substring(1, s.length-1)}) ~ withPos ^^ tChar

  def pStringLiteral = withPos ~ stringLiteral ~ withPos ^^ tString

  def pIntLiteral = withPos ~ wholeNumber ~ withPos ^^ {case p1 ~ s ~ p2 => tInt(this.~(this.~(p1, s.toInt), p2))}

  def pToken = rep(pIdentifier | pSep | pOp | pIntLiteral | pStringLiteral | pCharLiteral) <~ pComment.?

  def tokenizeALine(line: CharSequence, shift: Int): ParseResult[Stream[PositionedToken]] = {
    val reader = new ShiftReader(line, shift)
    parseAll(pToken, reader).map(_.toStream)
  }


  def tokenizeSource(lines: Iterator[CharSequence]): Either[(String, Int), Iterable[PositionedToken]] = {
    var result = ListBuffer[PositionedToken]()
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
