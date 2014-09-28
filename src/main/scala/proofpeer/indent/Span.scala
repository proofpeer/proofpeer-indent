package proofpeer.indent

/** A span corresponds to the area of the document consisting of the (non-whitespace) tokens from
  * firstTokenIndex to lastTokenIndex (both indices are inclusive). It measures various aspects of that 
  * area via its member variables. The members of Span correspond to the [[LayoutQualifier]] cases, 
  * except for firstTokenIndex and lastTokenIndex. 
  *
  * A span cannot represent an empty area or an area that does not begin and end with a
  * (non-whitespace) token. */
final case class Span(
  var firstRow : Int,
  var lastRow : Int,
  var leftMostInFirst : Int,
  var leftMost : Int,
  var leftMostFirst : Int,
  var leftMostRest : Int, // negative if not applicable
  var rightMostLast : Int, 
  var firstTokenIndex : Int,
  var lastTokenIndex : Int) 
{
    
  /** This assumes that s does not overlap with this span and actually comes ''behind'' it. */
  def addBehind(s : Span) {
    leftMostRest = 
      if (firstRow < s.firstRow) {
        if (leftMostRest < 0) 
          s.leftMost
        else if (leftMostRest <= s.leftMost) 
          leftMostRest 
        else s.leftMost
      } else s.leftMostRest
    lastRow = s.lastRow
    leftMost = if (leftMost <= s.leftMost) leftMost else s.leftMost
    rightMostLast = s.rightMostLast
    lastTokenIndex = s.lastTokenIndex
  }

  override def toString : String = {
    "[from row " + (firstRow + 1) +", column " + (leftMostFirst + 1) +", to row "+ (lastRow + 1) + ", column " + (rightMostLast + 1)+
      ", first non-whitespace column in first row: " + (leftMostInFirst + 1) + ", left most column: " + (leftMost + 1) + 
      ", left most column of tail: " + (leftMostRest + 1) + "]"
  }
    
}

final object Span {

  def apply(leftMostInRow : Int, row : Int, column : Int, tokenIndex : Int, len : Int = 1) : Span = {
    Span(row, row, leftMostInRow, column, column, -1, column + len - 1, tokenIndex, tokenIndex + len - 1)
  }

  def apply(span : Span) : Span = {
    import span._
    Span(firstRow, lastRow, leftMostInFirst, leftMost, leftMostFirst, leftMostRest, rightMostLast, firstTokenIndex, lastTokenIndex)
  }

  type Layout = Vector[Span] // null is treated as Vector()  

  def layoutsAreEqual(u : Layout, v : Layout) : Boolean = {
    if (u == null) return v == null
    if (v == null) return false
    val size = u.size
    if (size != v.size) return false
    for (i <- 0 until size) 
      if (u(i) != v(i)) return false
    return true
  }

  def addToLayout(layout : Layout, span : Span) : Layout = {
    if (layout == null) Vector(span)
    else layout :+ span
  }

  def spanOfLayout(layout : Layout) : Span = {
    if (layout == null) return null
    var span : Span = null
    for (s <- layout) {
      if (s != null) {
        if (span == null) 
          span = Span(s)
        else 
          span.addBehind(s)
      }
    }
    span
  }

}