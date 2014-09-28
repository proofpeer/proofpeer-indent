package proofpeer.indent.proofscript

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.earley._
import java.io.File


object Test {

  def read(f : File) : String = {
    val source = scala.io.Source.fromFile(f)
    val lines = source.mkString
    source.close()    
    lines
  }

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

    import Utils._
    import RegularExpr._
    //val dfa = ea.dfas(0)
    //val dfa = Literals.dfa(grammar.terminals)
    /*val r1 = char('<')
    val r2 = ALT(char(0x2264), string("<="))
    val r3 = ALT(char(0x2260), string("<>"))
    val nfa = NFA.fromRegularExprs(List((7, r1), (10, r2), (13, r3)))
    val dfa = DFA.fromNFA(nfa)*/
    //dfa.display()
    //val stream = CharacterStream.fromString("<>")
    //val (len, recognized) = DFA.run(dfa, stream, null)
    //println("len = "+len+", recognized = " + (recognized.map(ea.terminalOfId(_))))
    //println("len = "+len+", recognized = " + recognized.map(Literals.names(_)))
    //println("len = "+len+", recognized = " + recognized)



    
    val earley = new Earley(ea)
    val PROG = ea.idOfNonterminal("Prog")
    println("PROG = " + PROG)
    //val f = new File("/Users/stevenobua/myrepos/proofpeer-proofscript/scripts/bootstrap/conversions.thy")
    val f = new File("/Users/stevenobua/myrepos/proofpeer-hollight/proofscript/Lib.thy")
    val document = Document.fromString(read(f))
    val t3 = System.currentTimeMillis()
    val recognized = earley.recognize(document, Set(PROG))
    val t4 = System.currentTimeMillis()
    println("recognized = " + recognized)
    println("parsed in " + (t4 - t3) + " ms")
  }

}