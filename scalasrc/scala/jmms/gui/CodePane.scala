package jmms.gui

import java.awt.{Color, Dimension}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.{AttributeSet, DefaultStyledDocument, StyleConstants, StyleContext}
import javax.swing.{JFrame, JScrollPane, JTextPane}

import jmms.Tokenizer
import jmms.JToken._


/**
  * Created by weijiayi on 18/10/2016.
  */
class CodePane {
  import CodePane.transparent

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

  val pane = new JTextPane(document) {
    setForeground(transparent)
    setSelectedTextColor(transparent)
    setDisabledTextColor(transparent)
  }

  def code: String = document.getText(0, document.getLength)

  def setAttr(attributeSet: AttributeSet, offset: Int, length: Int, replace: Boolean) = {
    document.setCharacterAttributes(offset, length, attributeSet, replace)
//    document
  }

  def callback(): Unit = {
    import CodePane.colorAtrr
    val commentColor = colorAtrr(Color.gray)
    val basicColor = colorAtrr(Color.black)
    val reserveColor = colorAtrr(Color.blue.darker())
    val intColor = colorAtrr(Color.pink.darker())
    val stringColor = colorAtrr(Color.green.darker())
    val sepColor = reserveColor
    val opColor = colorAtrr(Color.orange)
    val charColor = colorAtrr(Color.orange)
    val errorColor = colorAtrr(Color.red)

    setAttr(commentColor, 0, document.getLength, replace = false)

    Tokenizer.tokenizeSource(code.lines) match {
      case Right(tokens) => tokens.foreach(t =>{
        val colorAttribute = t match{
          case _: TReserve => reserveColor
          case _: TInt => intColor
          case _: TString => stringColor
          case _: TChar => charColor
          case _: TSep => sepColor
          case _: TOp => opColor
          case _ => basicColor
        }
        val (start, until) = t.getRange
        setAttr(colorAttribute, start, until-start, replace = false)
      })
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


  def main(args: Array[String]): Unit = {
    import scala.io.Source

    val frame = new JFrame(){
      val codePane = new CodePane()
      val textCode = ""
      codePane.pane.setText(Source.fromFile("tests/pass/Animalia.java").mkString)
      setContentPane(new JScrollPane(codePane.pane) { setPreferredSize(new Dimension(600,600)) })

      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      pack()
      setVisible(true)
    }
  }

}