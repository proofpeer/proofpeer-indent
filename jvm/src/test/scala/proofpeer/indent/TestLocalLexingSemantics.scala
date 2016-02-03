package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._
import proofpeer.indent.earley.Earley
import proofpeer.indent.lr1._


object TestLocalLexingSemantics extends Properties("LocalLexingSemantics") {

  import Constraint._

  val grammar1 =
    rule("A", char('a'), None, "default") ++
    rule("E", EMPTY, None, FALLBACK_SCOPE) ++
    rule("S", "E E A", c => "EEA") ++
    rule("S", "E A A", c => "EAA") 
  val parser1 = Parser(grammar1)
  val lr1 = new LR1Parser(grammar1, "S")

  val grammar2 =
    rule("A", char('a'), None, "default") ++
    rule("E", EMPTY, None, FALLBACK_SCOPE) ++
    rule("S", "P P A", c => "PPA") ++
    rule("S", "P A A", c => "PAA") ++
    rule("P", "E", c => "P")
  val parser2 = Parser(grammar2)
  val lr2 = new LR1Parser(grammar2, "S")


  def isLR1(grammar : Grammar, startsymbol : String, numStates : Int) : Boolean = {
    val lr1a = new LR1Automaton(grammar, startsymbol)
    //println("number of states = " + lr1a.graph.size)
    lr1a.consistent && lr1a.graph.size == numStates
  }

  property("g1_a") = parser1.parse[String]("S", "a") == None
  property("g1_aa") = parser1.parse[String]("S", "aa") == Some("EAA")
  property("g1_incompatible") = grammar1.compatibilityErrors.size == 1
  property("g1_is_lr1") = isLR1(grammar1, "S", 6)
  property("g1_a_lr1") = lr1.parse[String]("a") == None
  property("g1_aa_lr1") = lr1.parse[String]("aa") == Some("EAA")

  property("g2_a") = parser2.parse[String]("S", "a") == Some("PPA")
  property("g2_aa") = parser2.parse[String]("S", "aa") == Some("PAA")
  property("g2_incompatible") = grammar2.compatibilityErrors.size == 1
  property("g2_is_lr1") = isLR1(grammar2, "S", 8)
  property("g2_a_lr1") = lr2.parse[String]("a") == None
  property("g2_aa_lr1") = lr2.parse[String]("aa") == Some("PAA")

}