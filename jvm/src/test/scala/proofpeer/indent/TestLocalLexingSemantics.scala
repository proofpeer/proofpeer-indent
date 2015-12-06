package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._
import proofpeer.indent.earley.Earley

object TestLocalLexingSemantics extends Properties("LocalLexingSemantics") {

  import Constraint._

  val grammar1 =
    rule("A", char('a'), None, "default") ++
    rule("E", EMPTY, None, FALLBACK_SCOPE) ++
    rule("S", "E E A", c => "EEA") ++
    rule("S", "E A A", c => "EAA") 
  val parser1 = Parser(grammar1)

  val grammar2 =
    rule("A", char('a'), None, "default") ++
    rule("E", EMPTY, None, FALLBACK_SCOPE) ++
    rule("S", "P P A", c => "PPA") ++
    rule("S", "P A A", c => "PAA") ++
    rule("P", "E", c => "P")
  val parser2 = Parser(grammar2)


  property("g1_a") = parser1.parse[String]("S", "a") == None
  property("g1_aa") = parser1.parse[String]("S", "aa") == Some("EAA")

  property("g2_a") = parser2.parse[String]("S", "a") == Some("PPA")
  property("g2_aa") = parser2.parse[String]("S", "aa") == Some("PAA")

}