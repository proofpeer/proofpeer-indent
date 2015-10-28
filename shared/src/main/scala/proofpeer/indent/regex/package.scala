package proofpeer.indent

package object regex {

  import proofpeer.general.StringUtils

  /** The type of regular expressions. */
  sealed trait RegularExpr
  
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
  
  def chars(c1 : Char, c2 : Char) : RegularExpr = CHAR(Range(c1, c2))

  def chars(c1 : Int, c2 : Int) : RegularExpr = CHAR(Range(c1, c2))

  def char(c : Char) : RegularExpr = chars(c, c)

  def char(c : Int) : RegularExpr = CHAR(Range(c))

  def caseInsensitiveChar(c : Int) : RegularExpr = {
    val lo = StringUtils.toLowerCase(c)
    val hi = StringUtils.toUpperCase(c)
    if (lo != hi) ALT(char(lo), char(hi)) else char(c)
  }

  def string(s : String, caseSensitive : Boolean = true) : RegularExpr = {
    val codes = StringUtils.codePoints(s)
    if (caseSensitive) {
      seq(codes.map(c => CHAR(Range(c))) : _*)
    } else {
      seq(codes.map(caseInsensitiveChar _) : _*)
    }
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

  def anything : RegularExpr = REPEAT1(CHAR(Range.universal))

  /** @return true if expr matches the empty string. */
  def matchesEmpty(expr : RegularExpr) : Boolean = {
    expr match {
      case NOTHING => false
      case EMPTY => true
      case CHAR(_) => false
      case ALT(left, right) => matchesEmpty(left) || matchesEmpty(right)
      case SEQ(first, second) => matchesEmpty(first) && matchesEmpty(second)
      case OPT(_) => true
      case REPEAT(_) => true
      case REPEAT1(expr) => matchesEmpty(expr)
    }
  }

  /** @return the range of characters which can start expr */
  def firstRange(expr : RegularExpr) : Range = {
    expr match {
      case NOTHING => Range.empty
      case EMPTY => Range.empty
      case CHAR(r) => r
      case ALT(left, right) => firstRange(left) + firstRange(right)
      case SEQ(first, second) => 
        if (matchesEmpty(first)) firstRange(first) + firstRange(second)
        else firstRange(first)
      case OPT(expr) => firstRange(expr)
      case REPEAT(expr) => firstRange(expr)
      case REPEAT1(expr) => firstRange(expr)
    }
  }


}

