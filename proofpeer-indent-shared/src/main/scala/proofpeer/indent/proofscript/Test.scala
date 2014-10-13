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

  def doEarley(grammar : Grammar, document : Document) {
    val t1 = System.currentTimeMillis()
    val ea = new EarleyAutomaton(grammar)
    val t2 = System.currentTimeMillis()
    println("Computed earley automaton in " + (t2 - t1) + " ms (it has " + ea.coreItems.size +" states)")
    //val absea = new HyperEarleyAutomaton(ea, ea.idOfNonterminal("Prog"))
    val earley = new Earley(ea)
    val t3 = System.currentTimeMillis()
    earley.parse(document, "Prog") match {
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
  }

  def doHyperEarley(grammar : Grammar, document : Document) {
    val t1 = System.currentTimeMillis()
    val ea = new EarleyAutomaton(grammar)
    val hea = new HyperEarleyAutomaton(ea)
    val t2 = System.currentTimeMillis()
    println("Computed hyper earley automaton in " + (t2 - t1) + " ms (it has " + hea.hyperCoreItems.size +" states)")
    val earley = new HyperEarley(hea)
    val t3 = System.currentTimeMillis()
    earley.parse(document, "Prog") match {
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
    val f = new File("/Users/stevenobua/myrepos/proofpeer-proofscript/scripts/bootstrap/redundancies.thy")
    //val f = new File("/Users/stevenobua/myrepos/proofpeer-hollight/proofscript/Lib.thy")
    val document = Document.fromString(read(f))
    println("size of document: " + document.size)
    doEarley(grammar, document)
  }

}