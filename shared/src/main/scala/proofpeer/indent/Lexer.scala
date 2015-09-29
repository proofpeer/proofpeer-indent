package proofpeer.indent

import proofpeer.indent.regex._

trait Lexer {
  /** Returns the maximum number of characters that the lexer accepts. A return value
      of 0 or less means that lexing failed. */
  def lex(d : Document, startPosition : Int, param : Int) : Int
}

object Lexer {

  private def makeDFA(regex : RegularExpr) : DFA = {
    val nfa = NFA.fromRegularExprs(List((0, regex)))
    DFA.fromNFA(nfa)
  }

  private class LexerImpl(dfa : DFA, stream : DocumentCharacterStream, 
    paramIsLowerColumnBound : Boolean) extends Lexer
  {
    def lex(d : Document, startPosition : Int, param : Int) : Int = {
      stream.init(d, startPosition, if (paramIsLowerColumnBound) param else -1)
      val (len, _) = DFA.run(dfa, stream, null)
      len
    }
  } 

  private class ThreadsafeLexerImpl(dfa : DFA, paramIsLowerColumnBound : Boolean, 
    ok : (Int, Int, Int, Int) => Boolean) extends Lexer
  {
    def lex(d : Document, startPosition : Int, param : Int) : Int = {
      val stream = new DocumentCharacterStream(ok)
      stream.init(d, startPosition, if (paramIsLowerColumnBound) param else -1)
      val (len, _) = DFA.run(dfa, stream, null)
      len
    }
  } 

  private def makeLexer(regex : RegularExpr, paramIsLowerColumnBound : Boolean,
    threadsafe : Boolean, ok : (Int, Int, Int, Int) => Boolean) : Lexer =
  {
    val dfa = makeDFA(regex)
    if (threadsafe) new ThreadsafeLexerImpl(dfa, paramIsLowerColumnBound, ok)
    else new LexerImpl(dfa, new DocumentCharacterStream(ok), paramIsLowerColumnBound)
  }

  def untilWhitespace(regex : RegularExpr, paramIsLowerColumnBound : Boolean, threadsafe : Boolean = true) : Lexer = 
    makeLexer(regex, paramIsLowerColumnBound, threadsafe, (r1, c1, r2, c2) => r1 == r2 && c2 == c1 + 1)

  def untilNewline(regex : RegularExpr, paramIsLowerColumnBound : Boolean, threadsafe : Boolean = true) : Lexer = 
    makeLexer(regex, paramIsLowerColumnBound, threadsafe, (r1, c1, r2, c2) => r1 == r2)

  def untilEmptyLine(regex : RegularExpr, paramIsLowerColumnBound : Boolean, threadsafe : Boolean = true) : Lexer = 
    makeLexer(regex, paramIsLowerColumnBound, threadsafe, (r1, c1, r2, c2) => r2 - r1 < 2)

  def untilEnd(regex : RegularExpr, paramIsLowerColumnBound : Boolean, threadsafe : Boolean = true) : Lexer = 
    makeLexer(regex, paramIsLowerColumnBound, threadsafe, (r1, c1, r2, c2) => true)

}