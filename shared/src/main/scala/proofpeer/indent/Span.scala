package proofpeer.indent

/** A span corresponds to the area of the document consisting of the (non-whitespace) tokens from
  * firstTokenIndex to lastTokenIndex (both indices are inclusive). It measures various aspects of that 
  * area via its member variables. The members of Span correspond to the [[Constraint.LayoutQualifier]] cases, 
  * except for firstTokenIndex and lastTokenIndex. 
  *
  * A span cannot represent an empty area or an area that does not begin and end with a
  * (non-whitespace) token. */
final case class Span(
  var firstRow : Int,
  var lastRow : Int,
  var maxRowGap : Int,
  var leftMostInFirst : Int,
  var leftMost : Int,
  var leftMostFirst : Int,
  var leftMostRest : Int, // negative if not applicable
  var rightMostLast : Int, 
  var firstIndexIncl : Int,
  var lastIndexExcl : Int) 
{
    
  /** This assumes that s does not overlap with this span and actually comes ''behind'' it. */
  def addBehind(s : Span) {
    if (firstRow < 0) {
      firstRow = s.firstRow
      lastRow = s.lastRow
      maxRowGap = s.maxRowGap
      leftMostInFirst = s.leftMostInFirst
      leftMost = s.leftMost
      leftMostFirst = s.leftMostFirst
      leftMostRest = s.leftMostRest
      rightMostLast = s.rightMostLast
      lastIndexExcl = s.lastIndexExcl
    } else {
      if (s.firstRow >= 0) {
        leftMostRest = 
          if (firstRow < s.firstRow) {
            if (leftMostRest < 0) 
              s.leftMost
            else if (leftMostRest <= s.leftMost) 
              leftMostRest 
            else s.leftMost
          } else s.leftMostRest
        var rowgap = s.firstRow - lastRow - 1
        if (s.maxRowGap > rowgap) rowgap = s.maxRowGap
        if (rowgap > maxRowGap) maxRowGap = rowgap 
        lastRow = s.lastRow
        leftMost = if (leftMost <= s.leftMost) leftMost else s.leftMost
        rightMostLast = s.rightMostLast
        lastIndexExcl = s.lastIndexExcl      
      } else {
        lastIndexExcl = s.lastIndexExcl
      }
    }
  }

  def isNull : Boolean = {
    firstRow < 0  
  }

  override def toString : String = {
    "[from row " + (firstRow + 1) +", column " + (leftMostFirst + 1) +", to row "+ (lastRow + 1) + ", column " + (rightMostLast + 1)+
      ", first non-whitespace column in first row: " + (leftMostInFirst + 1) + ", left most column: " + (leftMost + 1) + 
      ", left most column of tail: " + (leftMostRest + 1) + "]"
  }
    
}

final object Span {

  def apply(leftMostInRow : Int, row : Int, column : Int, firstIndex : Int, len : Int = 1) : Span = {
    Span(row, row, 0, leftMostInRow, column, column, -1, column + len - 1, firstIndex, firstIndex + len)
  }

  def apply(span : Span) : Span = {
    import span._
    Span(firstRow, lastRow, maxRowGap, leftMostInFirst, leftMost, leftMostFirst, leftMostRest, rightMostLast, firstIndexIncl, lastIndexExcl)
  }

  def nullSpan(firstIndexIncl : Int, lastIndexIncl : Int) : Span = {
    Span(-1, -1, -1, -1, -1, -1, -1, -1, firstIndexIncl, lastIndexIncl)
  }

  type Layout = Vector[Span] 

  def layoutsAreEqual(u : Layout, v : Layout) : Boolean = {
    val size = u.size
    if (size != v.size) return false
    for (i <- 0 until size) 
      if (u(i) != v(i)) return false
    return true
  }

  def emptyLayout(firstIndexIncl : Int, includes : Vector[Boolean]) : Layout = {
    if (includes.size > 0) Vector()
    else Vector(Span.nullSpan(firstIndexIncl, firstIndexIncl))
  }

  def addToLayout(firstTokenIncl : Int, includes : Vector[Boolean], 
    layout : Layout, span : Span) : Layout = 
  {
    if (span == null) throw new RuntimeException("cannot add null to layout")
    if (layout == null) throw new RuntimeException("layout cannot be null")
    val l = layout :+ span
    if (layout.size + 1 == includes.size)
      l :+ computeSpanOfLayout(firstTokenIncl, includes, l)
    else 
      l
  }

  private def computeSpanOfLayout(firstIndexIncl : Int, includes : Vector[Boolean], 
    layout : Layout) : Span = 
  {
    var span : Span = Span.nullSpan(firstIndexIncl, firstIndexIncl)
    var i = 0
    val size = includes.size
    while (i < size) {
      val s = layout(i)
      if (includes(i)) 
        span.addBehind(s) 
      else 
        span.addBehind(Span.nullSpan(s.firstIndexIncl, s.lastIndexExcl))
      i = i + 1
    }
    span
  }

  def getSpanOfLayout(layout : Layout) : Span = layout(layout.size - 1)

  def addSpans(s1 : Span, s2 : Span) : Span = {
    if (s1 != null && s2 != null) {
      val s = Span(s1)
      s.addBehind(s2)
      s
    } else {
      if (s2 != null) 
        Span(s2)
      else if (s1 != null)
        Span(s1)
      else 
        null
    }
  }

}