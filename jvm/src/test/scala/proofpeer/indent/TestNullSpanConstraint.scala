package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestNullSpanConstraint extends Properties("NullSpanConstraint") {

  import Constraint._

  val grammar =
    rule("Digits", REPEAT1(chars('0', '9')), Some(1), "numerals") ++
    rule("NINE", char('9'), Some(2), "numerals") ++
    rule("EIGHT", char('8'), Some(2), "numerals") ++
    rule("Num", "Digits", c => c.text("Digits")) ++
    rule("Num", "", c => "E") ++
    rule("cp", string("cp")) ++
    rule("A", "cp Num", not(NullSpan("Num")), c => c.Num[String]) ++
    rule("B", "cp Num", c => c.Num[String]) ++
    rule("X", "", Not(NullSpan("X")), c => "X") ++    
    rule("Y", "NINE X NINE", c => "9X9") ++
    rule("Y", "X", c => "X") ++
    rule("Z", "", forbidden, c => "Z1") ++
    rule("Z", "", unconstrained, c => "Z2") ++
    rule("N0", "EIGHT", c => "N0") ++
    rule("N1", "NINE", NullSpan("N1"), c => "N1") ++
    rule("N2", "Digits", unconstrained, c => "N2") ++
    rule("N", "N0", _.N0[String]) ++
    rule("N", "N1", _.N1[String]) ++
    rule("N", "N2", _.N2[String]) ++
    rule("M", "N0", forbidden, c => "N0") ++
    rule("M", "N2", c => "N2")

  val parser = Parser(grammar)

  def parse(nonterminal : String, s : String) = parser.parse[String](nonterminal, s)

  property("ex1") = parse("A", "cp 10") == Some("10")
  property("ex2") = parse("A", "cp") == None
  property("ex3") = parse("B", "cp 10") == Some("10")
  property("ex4") = parse("B", "cp") == Some("E")

  property("nullability1") = parse("X", "") == None
  property("nullability2") = parse("Y", "") == None
  property("nullability3") = parse("Y", "99") == None
  property("nullability4") = parse("Z", "") == Some("Z2")

  property("lexleak1") = parse("N", "8") == Some("N0")
  property("lexleak2") = parse("N", "9") == Some("N2")
  property("lexleak3") = parse("M", "8") == Some("N2")


}