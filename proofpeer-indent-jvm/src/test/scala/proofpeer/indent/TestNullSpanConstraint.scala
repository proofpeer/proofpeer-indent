package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestNullSpanConstraint extends Properties("NullSpanConstraint") {

  import Constraint._

  val grammar =
    rule("Digits", REPEAT1(chars('0', '9'))) ++
    rule("Num", "Digits", c => c.text("Digits")) ++
    rule("Num", "", c => "E") ++
    rule("cp", string("cp")) ++
    rule("A", "cp Num", not(NullSpan("Num")), c => c.Num[String]) ++
    rule("B", "cp Num", c => c.Num[String])

  val parser = Parser(grammar)

  def parse(nonterminal : String, s : String) = parser.parse[String](nonterminal, s)

  property("ex1") = parse("A", "cp 10") == Some("10")
  property("ex2") = parse("A", "cp") == None
  property("ex3") = parse("B", "cp 10") == Some("10")
  property("ex4") = parse("B", "cp") == Some("E")

}