package jmms

import cafebabe.AbstractByteCodes.{GetStatic, InvokeVirtual, Ldc}

import scala.io.Source

/**
  * Created by weijiayi on 19/10/2016.
  */
object Test {
  def main(args: Array[String]): Unit = {
    import jmms.{ExternalType, SemanticsAnalysis, SyntaxParser, TypeContext}
    import jmms.SBasicType.{SBoolean, SInt}

    def fullAnalyze(code: String) = {
      val semanP = new SemanticsAnalysis()
      val r = SyntaxParser.parsePackage(code)

      val sPackage = semanP.analyzeTypeContext(r, TypeContext.default())
      semanP.fullyAnalyze(sPackage)
      semanP.currentErrors().foreach(_.print())
      assert(semanP.currentErrors().isEmpty, "should be no errors")
      sPackage
    }

    val src =
      """
        |package Main;
        |import java.lang.String;
        |
        |class A {
        |  public static void main(int arg){
        |    int a = 3;
        |    a = a + 2;
        |  }
        |
        |  static int fib(int n){
        |    if(n<2)
        |      return 1;
        |    else
        |      return fib(n-1) + fib(n-2);
        |  }
        |
        |  private String jump(int height){
        |    return height.toString();
        |  }
        |
        |  public static boolean cool = true;
        |
        |  public A(boolean cool){
        |    this.cool = cool;
        |  }
        |}
      """.stripMargin

    val sPackage = fullAnalyze(src)
    val sb = sPackage.classes.head.localStaticMethods.values.find(_.signature.name == "fib").head.sBlock
    println(sb)

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
