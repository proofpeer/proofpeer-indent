package proofpeer.indent.proofscript

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.earley._


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
    val t1 = System.currentTimeMillis()
    val ea = new EarleyAutomaton(grammar)
    val t2 = System.currentTimeMillis()
    println("Computed earley automaton in " + (t2 - t1) + " ms.")
    val earley = new Earley(ea)
    val PROG = ea.idOfNonterminal("Prog")
    println("PROG = " + PROG)
    val document = Document.fromString("-13+7+ 8")
    val recognized = earley.recognize(document, Set(PROG))
    println("recognized = " + recognized)
  }

}