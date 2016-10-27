package jmm_tests

import jmms.SBasicType._
import jmms.SemanticTree.InternalType
import jmms._

/**
  * Created by weijiayi on 24/10/2016.
  */
class SemanticTests extends MyTest{

  "Internal Type" should{
    "answer inheritance relation" in {
      val a = InternalType("pkg","A", isAbstract = false)
      assert(a.isChildOf(a))

      val b = InternalType("pkg", "B", isAbstract = false)
      b.superClass = Some(a)

      assert(b.isChildOf(a))
      assert(! a.isChildOf(b))
    }

    "pass common type tests" in {
      SInt + SInt shouldBe SInt
      SVoid + SInt shouldBe SInt
      SVoid + SNoType shouldBe SNoType
      SNoType + SInt shouldBe SNoType
      SInt + SNoType shouldBe SNoType
      SInt + SVoid shouldBe SInt
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
      val semanP = new SemanticsAnalysis()
      val r = SyntaxParser.parsePackage(code)

      val sPackage = semanP.analyzeTypeContext(r, TypeContext.default())
      assert(semanP.currentErrors().isEmpty, "Errors: \n" + semanP.currentErrors().mkString("\n"))
      sPackage
    }

    "correctly resolve java.lang.Object" in {
      val src =
        """
          |package Main;
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

      val sPackage = parseContext(src)
      val ctx = sPackage.typeContext

      val a = ctx.locals("A").asInstanceOf[InternalType]
      a.localStaticMethods("main" -> IndexedSeq(SInt)).signature.returns shouldBe SVoid
      a.localInstanceFields("a").signature.fieldType shouldBe SInt
      a.localInstanceMethods("jump", IndexedSeq(SInt)).signature.returns shouldBe ExternalType.string
      a.localStaticFields("cool").signature.fieldType shouldBe SBoolean
      assert{ a.constructors.get(IndexedSeq(SBoolean)).isDefined }
    }

  }

  "fully analyzing" should {

    "type check fibnacci example" in {
      val src =
        """
          |package Main;
          |
          |class A {
          |  public static void main(int arg){
          |    int a = 3;
          |    a = a + 2;
          |  }
          |
          |  static int base = 1;
          |
          |  static int fib(int n){
          |    if(n<2)
          |      return A.base;
          |    else
          |      return fib(n-1) + fib(n-2);
          |  }
          |}
        """.stripMargin

      val sPackage = fullyAnalyze(src)
      val sb = sPackage.classes.head.localStaticMethods.values.find(_.signature.name == "fib").head.sBlock
      println(sb)
    }

    "type check example files" in {
      passSources.foreach{
        case (src, srcName) =>
          val sPackage = fullyAnalyze(src.mkString, srcName)

      }
    }

  }










}
