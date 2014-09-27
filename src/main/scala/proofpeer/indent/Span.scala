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
  var rightMostLast : Int, // inclusive
  var firstTokenIndex : Int,
  var lastTokenIndex : Int) // exclusive
{
    
  def isEmpty : Boolean = firstTokenIndex == lastTokenIndex

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
    
}

final object Span {

  def apply(leftMostInRow : Int, row : Int, column : Int, tokenIndex : Int) : Span = {
    Span(row, row, leftMostInRow, column, column, -1, column, tokenIndex, tokenIndex)
  }

  def apply(span : Span) : Span = {
    import span._
    Span(firstRow, lastRow, leftMostInFirst, leftMost, leftMostFirst, leftMostRest, rightMostLast, firstTokenIndex, lastTokenIndex)
  }

}