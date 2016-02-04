package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._
import proofpeer.indent.lr1._

object TestLR1LocalLexing extends Properties("LR1LocalLexing") {

  def grule(s : String, rhs : String) : Grammar = 
    rule(s, rhs, c => null)

  val G1 = 
    rule("SEMICOLON", char(';')) ++
    rule("COMMA", char(',')) ++
    rule("A", char('a')) ++    
    rule("OPEN", char('(')) ++
    rule("CLOSE", char(')')) ++
    grule("L", "L SEMICOLON E") ++
    grule("L", "E") ++
    grule("E", "E COMMA P") ++
    grule("E", "P") ++
    grule("P", "A") ++
    grule("P", "OPEN M CLOSE") ++
    grule("M", "") ++
    grule("M", "L") 
  val G1a = new LR1Automaton(G1, "L")

  val G4 = 
    rule("COMMA", char(',')) ++
    rule("A", char('a')) ++    
    rule("OPEN", char('(')) ++
    rule("CLOSE", char(')')) ++
    grule("L", "L COMMA E") ++
    grule("L", "E") ++
    grule("E", "A") ++
    grule("E", "OPEN L CLOSE") 
  val G4a = new LR1Automaton(G4, "L")

  val G5 = 
    rule("a", char('a')) ++
    rule("b", char('b')) ++    
    rule("c", char('c')) ++
    grule("S", "A B") ++
    grule("A", "a") ++
    grule("A", "a a") ++
    grule("B", "a C b") ++
    grule("C", "") ++
    grule("C", "c")  
  val G5a = new LR1Automaton(G5, "S")

  property("g1_consistent") = G1a.consistent
  property("g1_has_23_states") = G1a.graph.size == 23

  property("g4_consistent") = G4a.consistent
  property("g4_has_16_states") = G4a.graph.size == 16

  property("g5_inconsistent") = !G5a.consistent

}