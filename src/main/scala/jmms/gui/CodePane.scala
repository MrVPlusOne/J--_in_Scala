package jmms.gui

import java.awt.{Color, Dimension}
import javax.swing.event.{CaretEvent, CaretListener, DocumentEvent, DocumentListener}
import javax.swing.text.{AttributeSet, DefaultStyledDocument, StyleConstants, StyleContext}
import javax.swing._

import jmms.{SyntaxParser, SyntaxTree, Tokenizer}
import jmms.JToken._
import jmms.SyntaxParser.JTokenReader
import rx.{Ctx, Rx, Var}
import RxJComponent._
import jmms.SyntaxTree.{ClassDecl, ConstructorDecl, MethodMemberDecl}


/**
  * Created by weijiayi on 18/10/2016.
  */
class CodePane(implicit ctx: Ctx.Owner) {
  import CodePane.transparent

  val syntaxTree: Var[Option[SyntaxTree]] = Var(None)
  val caretDot: Var[Int] = Var(0)

  val document = new DefaultStyledDocument(){
    override def insertString(offs: Int, str: String, a: AttributeSet): Unit = {
      super.insertString(offs, str, a)
      onInsert()
    }

    override def remove(offs: Int, len: Int): Unit = {
      super.remove(offs, len)
      onRemove()
    }
  }

  val codePane = new JTextPane(document) {
    setForeground(transparent)
    setSelectedTextColor(transparent)
    setDisabledTextColor(transparent)
  }

  codePane.addCaretListener(new CaretListener {
    override def caretUpdate(e: CaretEvent): Unit = {
      caretDot() = e.getDot
    }
  })

  val locationString = Rx{
    (caretDot(), syntaxTree()) match{
      case (dot, Some(t)) =>
        val path = SyntaxTree.pathToDotInSrc(t,dot).reverse
        "Cursor: " + path.collect {
          case c: ClassDecl => c.name.data
          case m: MethodMemberDecl => m.name.data
          case c: ConstructorDecl => "Constructor"
        }.mkString(" |> ")
      case _ => "Parsing error"
    }
  }
  val locationPane = new RxJLabel(locationString, ctx)

  def code: String = document.getText(0, document.getLength)

  def setAttr(attributeSet: AttributeSet, offset: Int, length: Int, replace: Boolean) = {
    document.setCharacterAttributes(offset, length, attributeSet, replace)
//    document
  }

  def callback(): Unit = {
    import CodePane._
    val commentColor = colorAtrr(Color.gray)
    val basicColor = colorAtrr(Color.black)
    val reserveColor = colorAtrr(Color.blue.darker())
    val intColor = colorAtrr(Color.pink.darker())
    val stringColor = colorAtrr(Color.green.darker())
    val sepColor = reserveColor
    val opColor = colorAtrr(Color.orange)
    val charColor = colorAtrr(Color.orange)
    val errorColor = colorAtrr(Color.red)

//    val emptyBackground = backgroundAtrr(Color.white)
//    val errorAttr = backgroundAtrr(Color.red)

    setAttr(commentColor, 0, document.getLength, replace = true)
//    setAttr(emptyBackground, 0, document.getLength, replace = false)

    Tokenizer.tokenizeSource(code) match {
      case Right(tokens) =>
        tokens.foreach(t => {
          val colorAttribute = t match {
            case _: TReserve => reserveColor
            case _: TInt => intColor
            case _: TString => stringColor
            case _: TChar => charColor
            case _: TSep => sepColor
            case _: TOp => opColor
            case _ => basicColor
          }
          val (start, until) = t.range.get
          setAttr(colorAttribute, start, until - start, replace = false)
        })

        SyntaxParser.parseAll(SyntaxParser.pCompilationUnit ,JTokenReader(tokens, code.length)) match{
          case SyntaxParser.Success(r, _) =>
            syntaxTree() = Some(r)
          case fail: SyntaxParser.NoSuccess =>
            syntaxTree() = None
            val errorPos = {
              fail.next.asInstanceOf[JTokenReader].previous.getRange._2
            }
            setAttr(errorStyle, errorPos-2, errorPos-1 , replace = false)
        }


      case Left((msg, offset)) =>
        setAttr(errorColor, offset, code.length, replace = false)
        println(msg)
    }
  }

  def onInsert() = {
    callback()
  }

  def onRemove() = {
    callback()
  }

}

object CodePane {
  val transparent = new Color(0,0,0,0)

  private val context = StyleContext.getDefaultStyleContext
  def colorAtrr(color: Color) = context.addAttribute(context.getEmptySet, StyleConstants.Foreground, color)
  val errorStyle = context.addAttribute(context.getEmptySet, StyleConstants.Background, Color.red)

  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val frame = new JFrame(){
      import rx.Ctx.Owner.Unsafe._

      val codePane = new CodePane()
      val textCode = ""
      codePane.codePane.setText(Source.fromFile("tests/pass/Animalia.java").mkString)

      val content = new JPanel(){
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
        add(new JScrollPane(codePane.codePane) { setPreferredSize(new Dimension(600,600)) })
        add(new JScrollPane(codePane.locationPane))
      }

      setContentPane(content)

      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      pack()
      setVisible(true)
    }
  }

}