package proofpeer.indent

import proofpeer.indent.regex._

trait Lexer {
  /** Returns the maximum number of characters that the lexer accepts. A return value
      of 0 or less means that lexing failed. */
  def lex(d : Document, startPosition : Int, param : ParseParam.V) : Int
}

object Lexer {

  private def makeDFA(regex : RegularExpr) : DFA = {
    val nfa = NFA.fromRegularExprs(List((0, regex)))
    DFA.fromNFA(nfa)
  }

  private def param2bounds(param : ParseParam.V) : (Int, Int) = {
    import ParseParam._
    param match {
      case NIL => (Int.MinValue, Int.MaxValue)
      case INT(lcb) => (lcb, Int.MaxValue)
      case LIST(INT(lcb), NIL) => (lcb, Int.MaxValue)
      case LIST(INT(lcb), LIST(INT(ucsb), NIL)) => (lcb, ucsb)
      case _ => throw new RuntimeException("invalid lexer parameter: " + param)
    }
  }

  private class LexerImpl(dfa : DFA, ok : (Int, Int, Int, Int) => Boolean) extends Lexer
  {
    def lex(d : Document, startPosition : Int, param : ParseParam.V) : Int = {
      val (lcb, ucsb) = param2bounds(param)
      val stream = new DocumentCharacterStream(d, startPosition, ok, lcb, ucsb)
      val (len, _) = DFA.run(dfa, stream, null)
      len
    }
  } 

  private def makeLexer(regex : RegularExpr, ok : (Int, Int, Int, Int) => Boolean) : Lexer =
  {
    val dfa = makeDFA(regex)
    new LexerImpl(dfa, ok)
  }

  def untilWhitespace(regex : RegularExpr) : Lexer = 
    makeLexer(regex, (r1, c1, r2, c2) => r1 == r2 && c2 == c1 + 1)

  def untilNewline(regex : RegularExpr) : Lexer = 
    makeLexer(regex, (r1, c1, r2, c2) => r1 == r2)

  def untilEmptyLine(regex : RegularExpr) : Lexer = 
    makeLexer(regex, (r1, c1, r2, c2) => r2 - r1 < 2)

  def untilEnd(regex : RegularExpr) : Lexer = 
    makeLexer(regex, (r1, c1, r2, c2) => true)

}

final class DocumentCharacterStream(document : Document, startPosition : Int, 
  ok : (Int, Int, Int, Int) => Boolean, lowerColumnBound : Int, upperColumnStartBound : Int) 
extends CharacterStream {

  private val size = document.size
  private var position = startPosition
  private var lastRow = -1
  private var lastColumn = -1

  def nextCharacter : Int = {
    if (position >= size) return -1
    val (row, column, code) = document.character(position)
    if (column < lowerColumnBound) return -1
    if (lastColumn < 0 && column > upperColumnStartBound) return -1
    if (lastRow < 0 || ok(lastRow, lastColumn, row, column)) {
      lastRow = row
      lastColumn = column
      position += 1
      code      
    } else -1
  }

}