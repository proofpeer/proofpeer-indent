package proofpeer.indent.regex

/** The type of regular expressions. */
sealed trait RegularExpr

object RegularExpr {

  /** This matches nothing. */
  case object NOTHING extends RegularExpr

  /** ε, matches the empty string */
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

object Utils {

  import RegularExpr._

  def chars(c1 : Char, c2 : Char) : RegularExpr = CHAR(Range(c1, c2))

  def chars(c1 : Int, c2 : Int) : RegularExpr = CHAR(Range(c1, c2))

  def char(c : Char) : RegularExpr = chars(c, c)

  def char(c : Int) : RegularExpr = CHAR(Range(c))

  def string(s : String) : RegularExpr = {
    val codes = proofpeer.general.StringUtils.codePoints(s)
    seq(codes.map(c => CHAR(Range(c))) : _*)
  }

  def seq(rs : RegularExpr*) : RegularExpr = {
    var range : RegularExpr = EMPTY
    for (r <- rs)range = SEQ(range, r)
    range
  }

  def alt(rs : RegularExpr*) : RegularExpr = {
    var range : RegularExpr = NOTHING
    for (r <- rs) range = ALT(range, r)
    range
  }

}

