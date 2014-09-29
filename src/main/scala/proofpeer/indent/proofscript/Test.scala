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
    println("Number of terminals: " + grammar.terminals.size)
    ea.dfas(0).display()
    val earley = new Earley(ea)
    val PROG = ea.idOfNonterminal("Prog")
    println("PROG = " + PROG)
    //val f = new File("/Users/stevenobua/myrepos/proofpeer-proofscript/scripts/bootstrap/conversions.thy")
    val f = new File("/Users/stevenobua/myrepos/proofpeer-hollight/proofscript/Lib.thy")
    val document = Document.fromString(read(f))
    val t3 = System.currentTimeMillis()
    earley.parse(document, PROG) match {
      case Left(parsetree) =>
        if (parsetree.hasAmbiguities) 
          println("ambiguous parse")
        else
          println("parsed successfully")
      case Right(k) =>
        val (row, column, code) = document.character(k)
        val c : Char = code.toChar
        println("parse error at position "+k+" in row "+(row + 1)+", column "+(column + 1)+" at character code " + code + " = '" + c +"'")        
    }
    val t4 = System.currentTimeMillis()
    println("parsed in " + (t4 - t3) + " ms")
    println("number of bin additions: " + Measuring.addItem)
    println("number of DFA moves: " + Measuring.move)
    println("number of DFA final states encountered: " + Measuring.checkFinalState)
  }

}