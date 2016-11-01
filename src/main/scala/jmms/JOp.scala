package jmms

/**
  * Operators supported by j--
  */
object JOp extends Enumeration{
  val operatorChars = "=&|><+-*/!?~"
  val operatorRegex = """[=&|><+-[*]/!?~]+""".r

  // assignment
  val assign, assignPlus = Value

  // conditional
  val and, or = Value

  // equality
  val equal = Value

  // relational
  val greater, lessEqual, instanceOf = Value

  // arithmetic
  val plus, minus, multiply, divide = Value

  // unary
  val unary_plusplus, unary_minus, unary_not = Value

  // postfix
  val post_minusminus = Value

  val nameMap = Map(
    "=" -> assign, "+=" -> assignPlus,
    "&&" -> and, "||" -> or,
    "==" -> equal,
    ">" -> greater, "<=" -> lessEqual, "instanceof" -> instanceOf,
    "+" -> plus, "-" -> minus, "*" -> multiply, "/" -> divide,
    "++" -> unary_plusplus, "-" -> unary_minus, "!" -> unary_not,
    "--" -> post_minusminus
  )

  def fromName(name: String): Option[JOp.Value] = nameMap.get(name)
}
