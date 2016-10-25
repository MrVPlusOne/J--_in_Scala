package jmms

import cafebabe.AbstractByteCodes.{GetStatic, InvokeVirtual, Ldc}

import scala.io.Source

/**
  * Created by weijiayi on 19/10/2016.
  */
object Test {
  def main(args: Array[String]): Unit = {

  }

  def writeClassFile(): Unit ={
    import cafebabe.AbstractByteCodes.{GetStatic, InvokeVirtual, Ldc}
    import cafebabe.ByteCodes.RETURN
    import cafebabe._

    val classFile = new ClassFile("Test", None)
    classFile.addField("Ljava/lang/String;", "greeting")
    val main = classFile.addMainMethod.codeHandler

    main << GetStatic("java/lang/System","out","Ljava/io/PrintStream;") <<
      Ldc("Hi, there!") <<
      InvokeVirtual("java/io/PrintStream","println","(Ljava/lang/String;)V") <<
      RETURN

    main.freeze

    classFile.writeToFile("./Test.class")
  }
}
