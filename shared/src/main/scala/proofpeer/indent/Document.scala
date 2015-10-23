package proofpeer.indent

trait Document {
  
  def size : Int
  
  /** @return (row, column, characterCode) */
  def character(position : Int) : (Int, Int, Int)

  def firstPositionInRow(row : Int) : Int
  
  def getText(position : Int, len : Int) : String

  def span(position : Int, len : Int) : Span

  def getText(span : Span) : String = 
    if (span == null) ""
    else getText(span.firstIndexIncl, span.lastIndexExcl - span.firstIndexIncl)

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

  def span(position : Int, len : Int) : Span = {
    var (row, column, _) = characters(position)
    val (lastRow, lastColumn, _) = characters(position + len - 1)
    val (_, col0, _) = characters(rows(row))
    if (row == lastRow) {
      Span(col0, row, column, position, len)
    } else {
      var pos = rows(row + 1)
      var (currentRow, currentColumn, _) = characters(pos)
      var span = Span(col0, row, column, position, pos - position)
      var l = len - (pos - position)
      while (currentRow < lastRow) {
        val nextpos = rows(currentRow + 1)
        span.addBehind(Span(currentColumn, currentRow, currentColumn, pos, nextpos - pos))        
        val (nextRow, nextColumn, _) = characters(nextpos)
        l = l - (nextpos - pos)
        currentRow = nextRow
        currentColumn = nextColumn
        pos = nextpos
      }
      span.addBehind(Span(currentColumn, currentRow, currentColumn, pos, l))
      span
    }
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

  /** After parsing a line, accept is called with the line as its parameter (line will not contain any line termination characters).
    * If accept(line) is true, the line is added to the document and parsing continues; otherwise the line is not added and parsing stops.
    */
  def fromString(text : String, tab : Option[Int], accept : String => Boolean) : Document = {
    import proofpeer.general.StringUtils._
    var characters : List[(Int, Int, Int)] = List()
    var row = 0
    var column = 0
    var index = 0
    val size = text.length()
    var line : List[Int] = List()
    var linelength = 0
    def stopParsing() : Option[Document] = {
      val l = fromCodePoints(line.toVector.reverse)
      if (accept(l)) {
        line = List()
        linelength = 0
        None
      } else {
        val chars = characters.drop(linelength)
        Some(new UnicodeDocument(chars.reverse.toVector))
      }
    }
    while (index < size) {
      val code : Int = codePointAt(text, index)
      code match {
        case 13 /* CR */ => 
          stopParsing() match {
            case None =>
            case Some(d) => return d
          }
          if (index + 1 < size && text.charAt(index + 1) == 10) index += 2 else index += 1
          row += 1
          column = 0
        case 10 /* LF */ => 
          stopParsing() match {
            case None =>
            case Some(d) => return d
          }
          if (index + 1 < size && text.charAt(index + 1) == 13) index += 2 else index += 1
          row += 1
          column = 0
        case 32 /* SPACE */ =>
          index += 1
          column += 1 
          line = 32 :: line
        case 9 /* TAB */ if tab.isDefined =>
          index += 1
          column += tab.get
          for (i <- 1 to tab.get) line = 32 :: line
        case _ =>
          characters = (row, column, code) :: characters
          index += charCount(code)
          column += 1
          line = code :: line
          linelength += 1
      }
    }
    stopParsing() match {
      case None => new UnicodeDocument(characters.reverse.toVector)
      case Some(d) => d
    }
  }
  
}
