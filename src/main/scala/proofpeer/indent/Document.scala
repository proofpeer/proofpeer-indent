package proofpeer.indent

trait Document {
  
  def size : Int
  
  /** @return (row, column, characterCode) */
  def character(position : Int) : (Int, Int, Int)

  def firstPositionInRow(row : Int) : Int
  
  def getText(position : Int, len : Int) : String

  def getText(span : Span) : String = 
    if (span == null) ""
    else getText(span.firstTokenIndex, span.lastTokenIndex - span.firstTokenIndex + 1)

}

final class UnicodeDocument(characters : Vector[(Int, Int, Int)])  extends Document {
    
  def size = characters.size

  def character(position : Int) = characters(position)

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

  def firstPositionInRow(row : Int) : Int = rows(row)
    
  def getText(position : Int, len : Int) : String = {
    if (len == 0) return ""
    var (row, column, code) = characters (position)
    var codes : List[Int] = List(code)
    var i = 1
    while (i < len) {
      var (row2, column2, code) = characters (position + i)
      while (row < row2) {
        column = -1
        row = row + 1
        codes = '\n' :: codes
      }
      column = column + 1
      while (column < column2) {
        column = column + 1
        codes = ' '  :: codes
      } 
      codes = code :: codes
      i = i + 1
    }
    var arr = codes.reverse.toArray
    new String(arr, 0, arr.length)
  }
  
}

object Document {
  
  def fromString(text : String) : Document = fromString(text, None)
  
  def fromString(text : String, tab : Option[Int]) : Document = {
    import proofpeer.general.StringUtils._
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
