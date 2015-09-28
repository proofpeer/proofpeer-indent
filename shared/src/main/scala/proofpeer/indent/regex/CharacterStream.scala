package proofpeer.indent.regex

trait CharacterStream {

  /** @return -1 if the end of the document has been reached, otherwise the character code */
  def nextCharacter : Int 

}

object CharacterStream {

  private class StringCharacterStream(val s : String, var index : Int) extends CharacterStream
  {
    def nextCharacter : Int = {
      import proofpeer.general.StringUtils._
      if (index >= s.size) return -1
      val c = codePointAt(s, index)
      index += charCount(c)
      c
    }
  }

  def fromString(s : String) : CharacterStream = {
    new StringCharacterStream(s, 0)
  }

}

import proofpeer.indent.Document

final class DocumentCharacterStream(ok : (Int, Int, Int, Int) => Boolean) extends CharacterStream {

  private var document : Document = null
  private var size = 0
  private var position = 0
  private var lastRow = -1
  private var lastColumn = -1
  private var lowerColumnBound = -1

  def nextCharacter : Int = {
    if (position >= size) return -1
    val (row, column, code) = document.character(position)
    if (column < lowerColumnBound) return -1
    if (lastRow < 0 || ok(lastRow, lastColumn, row, column)) {
      lastRow = row
      lastColumn = column
      position += 1
      code      
    } else -1
  }

  def init (d : Document, p : Int, lcb : Int = -1) {
    document = d
    size = document.size
    position = p
    lastRow = -1
    lastColumn = -1
    lowerColumnBound = lcb
  }

}