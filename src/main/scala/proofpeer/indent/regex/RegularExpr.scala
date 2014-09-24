package proofpeer.indent.regex

/** The type of regular expressions. */
sealed trait RegularExpr

object RegularExpr {

  /** ε */
  case object EMPTY extends RegularExpr

  /** All characters with unicode values in the given range. */
  case class CHAR(range : Range) extends RegularExpr

  /** left or right */
  case class ALT(left : RegularExpr, right : RegularExpr) extends RegularExpr
  
  /** left followed by right */
  case class SEQ(first : RegularExpr, second : RegularExpr) extends RegularExpr
  
  /** expr? */
  case class OPT(expr : RegularExpr) extends RegularExpr
  
  /** expr* */
  case class REPEAT(expr : RegularExpr) extends RegularExpr
  
  /** expr+ */
  case class REPEAT1(expr : RegularExpr) extends RegularExpr

  /** the space character */
  val SPACE = CHAR(Range(32))
  
  /** the newline character */
  val NEWLINE = CHAR(Range(10))
  
  /** any nonempty sequence of spaces and newlines */
  val WHITESPACE = REPEAT1(ALT(SPACE, NEWLINE))

}

