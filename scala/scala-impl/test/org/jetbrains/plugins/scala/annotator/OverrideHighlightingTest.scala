package org.jetbrains.plugins.scala.annotator

class OverrideHighlightingTest extends ScalaHighlightingTestBase {
  def testScl13051(): Unit = {
    val code =
      s"""
         |trait Base {
         |  def foo: Int = 42
         |}
         |
         |class AClass extends Base {
         |  override def foo: String = "42"
         |}
       """.stripMargin
    assertMatches(errorsFromScalaCode(code)) {
      case Error(_, "Overriding type String does not conform to base type Int") :: Nil =>
    }
  }

  def testScl13051_1(): Unit = {
    val code =
      s"""
         |trait T1 {
         |  val foo: T1
         |}
         |trait T2 extends T1 {
         |  override val foo: T2
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }
}
