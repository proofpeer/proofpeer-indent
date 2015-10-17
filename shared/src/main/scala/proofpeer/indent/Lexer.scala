package proofpeer.indent

import proofpeer.indent.regex._

trait Lexer {
  /** Returns the maximum number of characters that the lexer accepts. A return value
      of 0 or less means that lexing failed. */
  def lex(d : Document, startPosition : Int, param : ParseParam.V) : (Int, ParseParam.V)
}

object Lexer {

  private def makeDFA(regex : RegularExpr) : DFA = {
    val nfa = NFA.fromRegularExprs(List((0, regex)))
    DFA.fromNFA(nfa)
  }

  type Layout = ParseParam.V => ((Int, Int) => Boolean, (Int, Int, Int, Int) => Boolean)

  private def simpleLayout(ok : (Int, Int, Int, Int) => Boolean) : Layout = {
    (p : ParseParam.V) => {
      val List(minColumn, minRow, maxStartColumn, maxStartRow) =
        ParseParam.v2Ints(List(Int.MinValue, Int.MinValue, Int.MaxValue, Int.MaxValue), p)
      def start(row : Int, column : Int) : Boolean = 
        column >= minColumn && row >= minRow && row <= maxStartRow && column <= maxStartColumn
      def continue(lastRow : Int, lastColumn : Int, row : Int, column : Int) =
        column >= minColumn && ok(lastRow, lastColumn, row, column)
      (start _, continue _)
    }
  }

  private class LexerImpl(dfa : DFA, layout : Layout) extends Lexer
  {
    def lex(d : Document, startPosition : Int, param : ParseParam.V) : (Int, ParseParam.V) = {
      val (start, continue) = layout(param)
      val stream = new DocumentCharacterStream(d, startPosition, start, continue)
      val (len, _) = DFA.run(dfa, stream, null)
      (len, earley.Earley.DEFAULT_RESULT)
    }
  } 

  def make(regex : RegularExpr, layout : Layout) : Lexer =
  {
    val dfa = makeDFA(regex)
    new LexerImpl(dfa, layout)
  }

  def untilWhitespace(regex : RegularExpr) : Lexer = 
    make(regex, simpleLayout((r1, c1, r2, c2) => r1 == r2 && c2 == c1 + 1))

  def untilNewline(regex : RegularExpr) : Lexer = 
    make(regex, simpleLayout((r1, c1, r2, c2) => r1 == r2))

  def untilEmptyLine(regex : RegularExpr) : Lexer = 
    make(regex, simpleLayout((r1, c1, r2, c2) => r2 - r1 < 2))

  def untilEnd(regex : RegularExpr) : Lexer = 
    make(regex, simpleLayout((r1, c1, r2, c2) => true))

  def demandLeftBorder(lexer : Lexer, onlyNewline : Boolean) : Lexer = 
    new DemandLeftBorder(lexer, onlyNewline)

  def demandRightBorder(lexer : Lexer, onlyNewline : Boolean) : Lexer = 
    new DemandRightBorder(lexer, onlyNewline)
}

final class DocumentCharacterStream(document : Document, startPosition : Int,
  start : (Int, Int) => Boolean, continue : (Int, Int, Int, Int) => Boolean) 
extends CharacterStream {

  private val size = document.size
  private var position = startPosition
  private var lastRow = -1
  private var lastColumn = -1

  def nextCharacter : Int = {
    if (position >= size) return -1
    val (row, column, code) = document.character(position)
    if (lastRow < 0 && !start(row, column)) return -1
    if (lastRow >= 0 && !continue(lastRow, lastColumn, row, column)) return -1
    lastRow = row
    lastColumn = column
    position += 1
    code      
  }

}

final class DemandLeftBorder(lexer : Lexer, newline : Boolean) extends Lexer {

  def lex(d : Document, startPosition : Int, param : ParseParam.V) : (Int, ParseParam.V) = {
    var allow : Boolean = false
    if (startPosition <= 0 || startPosition >= d.size) allow = true
    else {
      val (row1, col1, _) = d.character(startPosition - 1)
      val (row2, col2, _) = d.character(startPosition)
      if (row1 != row2) allow = true
      else if (!newline && col1 + 1 < col2) allow = true
    }
    if (allow) lexer.lex(d, startPosition, param) else (-1, ParseParam.UNDEFINED)
  }

}

final class DemandRightBorder(lexer : Lexer, newline : Boolean) extends Lexer {

  def lex(d : Document, startPosition : Int, param : ParseParam.V) : (Int, ParseParam.V) = {
    val R = lexer.lex(d, startPosition, param)
    if (R._1 > 0) {
      val endPosition = startPosition + R._1 - 1
      if (endPosition + 1 >= d.size) return R
      val (row1, col1, _) = d.character(endPosition)
      val (row2, col2, _) = d.character(endPosition + 1)
      if (row1 != row2) return R
      if (!newline && col1 + 1 < col2) return R
      (-1, ParseParam.UNDEFINED)
    } else R
  }

}




