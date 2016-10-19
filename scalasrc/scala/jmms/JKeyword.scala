package jmms

/**
  * Reserve works in J--
  */
object JKeyword extends Enumeration{
  val k_abstract, k_extends, k_int, k_protected, k_this, k_boolean, k_false, k_new, k_public, k_true = Value
  val k_char, k_import, k_null, k_return, k_void, k_class, k_if, k_package, k_static, k_while = Value
  val k_else, k_instanceof, k_private, k_super = Value

  val keywordArray = Array("abstract", "extends", "int", "protected", "this", "boolean", "false", "new", "public", "true",
  "char", "import", "null", "return", "void", "class", "if", "package", "static", "while",
  "else", "instanceof", "private", "super")

  val string2keyword = keywordArray.zipWithIndex.toMap.mapValues(i => JKeyword.apply(i))
}
