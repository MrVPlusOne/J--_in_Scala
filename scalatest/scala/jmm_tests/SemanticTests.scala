package jmm_tests

import jmms.SBasicType._
import jmms.SemanticsParser.TypeContext
import jmms.SemanticsTree.InternalType
import jmms._

/**
  * Created by weijiayi on 24/10/2016.
  */
class SemanticTests extends MyTest{
  "Internal Type" should{
    "answer inheritance relation" in {
      val a = InternalType("pkg","A")
      assert(a.isChildOf(a))

      val b = InternalType("pkg", "B")
      b.superClass = Some(a)

      assert(b.isChildOf(a))
      assert(! a.isChildOf(b))
    }
  }

  "External Type" should {
    "resolve some methods" in {
      val toString = ExternalType.string.getMethod("toString", IndexedSeq()).get
      toString.isStatic shouldBe false
      toString.returns shouldBe ExternalType.string
    }
  }

  "Source File Context parsing" should{
    def parseContext(code: String) = {
      val semanP = new SemanticsParser()
      val r = SyntaxParser.parsePackage(code)

      val newContext = semanP.parseTypeContext(r, TypeContext.empty())
      assert(semanP.currentErrors().isEmpty, "Errors: \n" + semanP.currentErrors().mkString("\n"))
      newContext
    }

    "correctly resolve java.lang.Object" in {
      val src =
        """
          |package Main;
          |import java.lang.Object;
          |
          |class A extends java.lang.Object {
          |
          |}
        """.stripMargin

      parseContext(src)
    }

    "correctly resolve multiple classes" in {
      val src =
        """
          |package Main;
          |import java.lang.Object;
          |
          |class A extends C {
          |
          |}
          |class B {} class C extends B {}
        """.stripMargin

      parseContext(src)
    }

//    "detect circular inheritances" in {
//      val src =
//        """
//          |package Main;
//          |
//          |class A extends C {
//          |
//          |}
//          |class B extends A {}
//          |class C extends B {}
//        """.stripMargin
//
//      parseContext(src)
//    } todo: add it

    "resolve method members" in {
      val src =
        """
          |package Main;
          |import java.lang.String;
          |
          |class A {
          |  public static void main(int arg){
          |    int a = 3;
          |  }
          |  int a = 5;
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

      val ctx = parseContext(src)

      val a = ctx.locals("A").asInstanceOf[InternalType]
      a.localStaticMethods("main" -> IndexedSeq(SInt)).signature.returns shouldBe SVoid
      a.localInstanceFields("a").signature.tpe shouldBe SInt
      a.localInstanceMethods("jump", IndexedSeq(SInt)).signature.returns shouldBe ExternalType.string
      a.localStaticFields("cool").signature.tpe shouldBe SBoolean
      assert{ a.constructors.get(IndexedSeq(SBoolean)).isDefined }
    }
  }

}
