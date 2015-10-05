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

  type Layout = ParseParam.V => ((Int, Int) => Boolean, (Int, Int, Int, Int) => Boolean)

  private def simpleLayout(ok : (Int, Int, Int, Int) => Boolean) : Layout = {
    (p : ParseParam.V) => {
      val List(minColumn, maxStartColumn, maxStartRow) =
        ParseParam.v2Ints(List(Int.MinValue, Int.MaxValue, Int.MaxValue), p)
      def start(row : Int, column : Int) : Boolean = 
        column >= minColumn && row <= maxStartRow && column <= maxStartColumn
      def continue(lastRow : Int, lastColumn : Int, row : Int, column : Int) =
        column >= minColumn && ok(lastRow, lastColumn, row, column)
      (start _, continue _)
    }
  }

  private class LexerImpl(dfa : DFA, layout : Layout) extends Lexer
  {
    def lex(d : Document, startPosition : Int, param : ParseParam.V) : Int = {
      val (start, continue) = layout(param)
      val stream = new DocumentCharacterStream(d, startPosition, start, continue)
      val (len, _) = DFA.run(dfa, stream, null)
      len
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