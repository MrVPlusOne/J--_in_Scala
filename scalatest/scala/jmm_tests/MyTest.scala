package jmm_tests

import java.io.File

import org.scalatest.WordSpec

import scala.io.Source

/**
  * Created by weijiayi on 19/10/2016.
  */
trait MyTest extends WordSpec {
  implicit class pathString(p: String){
    def / (p1: String) = {
      if(p.endsWith("/")) p + p1
      else p + "/" + p1
    }
  }

  def testFileBaseDir: String = "tests"

  def passSources = {
    new File(testFileBaseDir / "pass").listFiles().
      filter(_.getName endsWith ".java").
      map(f => Source.fromFile(f))
  }

}
