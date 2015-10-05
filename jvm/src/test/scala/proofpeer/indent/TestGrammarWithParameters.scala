package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestGrammarWithParameters extends Properties("GrammarWithParameters") {

  import Constraint._

  val grammar =
    rule("Digits", REPEAT1(chars('0', '9'))) ++
    rule("Num", "Digits~", c => c.text("Digits")) ++
    rule("S", "Num(2)", c => c.Num[String])

  val parser = Parser(grammar)
  def parse(nonterminal : String, s : String) = parser.parse[String](nonterminal, s)

  property("indent0") = parse("S", "10") == None
  property("indent1") = parse("S", " 10") == None
  property("indent2") = parse("S", "  10") == Some("10")

}