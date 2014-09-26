package proofpeer.indent.proofscript

import proofpeer.indent._
import proofpeer.indent.regex._


object Test {

  def main(args : Array[String]) {
    val grammar = Syntax.grammar
    if (!grammar.isWellformed) {
      val errors = grammar.errors
      println("The ProofScript grammar contains " + errors.size + " errors: ")
      for (i <- 1 to errors.size) {
        println ("" + i +") " + errors(i - 1))
      }
      println("")
      return
    }
    val scanrules = grammar.scanrules
    val usedScanSymbols = scanrules.keys.toSet.intersect(grammar.usedSymbols)
    println("number of scan rules: " + scanrules.size)
    println("number of used scan symbols: " + usedScanSymbols.size)
    val scansymbols = usedScanSymbols.toVector
    var tokenId = 0
    var exprs : List[(Int, RegularExpr)] = List()
    for (symbol <- scansymbols) {
      exprs = (tokenId, scanrules(symbol).regex) :: exprs
      tokenId += 1
    }
    val t1 = System.currentTimeMillis
    val nfa = NFA.fromRegularExprs(exprs)
    val dfa = DFA.fromNFA(nfa)
    val t2 = System.currentTimeMillis
    println("number of DFA states: " + (dfa.maxState - dfa.startState + 1))
    println("time needed to compute DFA: " + (t2 - t1))
    def scan(s : String) {
      val stream = CharacterStream.fromString(s)
      val (len, ids) = DFA.run(dfa, stream)  
      println("scanning '" + s + "' ...")
      println("  scanned " + len + " characters")
      println("  tokens = " + ids.map(scansymbols(_)))    
    }
    scan("hey")
    scan("13")
    scan("hello world!")
    scan("hello\\u")
    scan("hello\\uFFFF")
  }

}