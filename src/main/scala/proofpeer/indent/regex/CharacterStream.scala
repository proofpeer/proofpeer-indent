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