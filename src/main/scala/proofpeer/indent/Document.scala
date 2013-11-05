package proofpeer.indent

/** The members of Span correspond to the [[Constraints.LayoutEntity]] cases. */
case class Span(
  firstRow : Int,
  lastRow : Int,
  leftMostInFirst : Int,
  leftMost : Int,
  leftMostFirst : Int,
  leftMostRest : Option[Int],
  rightMostLast : Int) 
{
    
  /** This assumes that s does not overlap with this span and actually comes ''behind'' it. */
  def addBehind(s : Span) : Span = {
    val _firstRow = firstRow
    val _lastRow = lastRow
    val _leftMostInFirst = leftMostInFirst
    val _leftMost = if (leftMost <= s.leftMost) leftMost else s.leftMost
    val _leftMostFirst = leftMostFirst
    val _rightMostLast = s.rightMostLast
    val _leftMostRest = 
      if (firstRow < s.firstRow) {
        leftMostRest match {
          case None => Some(s.leftMost)
          case Some(lmr) => Some(if (lmr <= s.leftMost) lmr else s.leftMost)
        }
      }
      else
        s.leftMostRest
    Span(_firstRow, _lastRow, _leftMostInFirst, _leftMost, _leftMostFirst, _leftMostRest, _rightMostLast)   
  }
    
}
  
case class Token(terminal : API.Terminal, i : Int, span : Span)

trait Document {
  
  def size : Int
  
  def getToken(position : Int) : Token

}

class UnicodeDocument(characters : Vector[(Int, Int, Int)])  extends Document {
    
  def size = characters.size

  private def computeRows() : Vector[Int] = {
    var rows : List[Int] = List()
    var currentRow = -1
    var position = 0
    for ((row, _, _) <- characters) {
      while (row > currentRow) {
        rows = position :: rows
        currentRow = currentRow + 1       
      }
      position = position + 1
    }
    rows.reverse.toVector
  }
  
  /** Points to the first position with greater or equal row */
  private val rows = computeRows()   
  
  def getToken(position : Int) : Token = {
    import API._
    val (row, column, code) = characters (position)
    val (_, column0, _) = characters(rows(row))
    val terminal = Terminal(SymbolNameCode(code)) 
    val span = Span(row, row, column0, column, column, None, column)
    Token(terminal, position, span)
  }
  
}

object UnicodeDocument {
  
  def fromString(text : String) : UnicodeDocument = fromString(text, None)
  
  def fromString(text : String, tab : Option[Int]) : UnicodeDocument = {
    import proofpeer.scala.lang._
    var characters : List[(Int, Int, Int)] = List()
    var row = 0
    var column = 0
    var index = 0
    val size = text.length()
    while (index < size) {
      val code : Int = codePointAt(text, index)
      code match {
        case 13 /* CR */ => 
          if (index + 1 < size && text.charAt(index + 1) == 10) index += 2 else index += 1
          row += 1
          column = 0
        case 10 /* LF */ => 
          if (index + 1 < size && text.charAt(index + 1) == 13) index += 2 else index += 1
          row += 1
          column = 0
        case 32 /* SPACE */ =>
          index += 1
          column += 1 
        case 9 /* TAB */ if tab.isDefined =>
          index += 1
          column += tab.get
        case _ =>
          characters = (row, column, code) :: characters
          index += charCount(code)
          column += 1
      }
    }
    new UnicodeDocument(characters.reverse.toVector)
  }
  
}
