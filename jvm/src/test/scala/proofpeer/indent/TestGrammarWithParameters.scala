package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestGrammarWithParameters extends Properties("GrammarWithParameters") {

  import Constraint._

  val grammar =
    rule("Digits", REPEAT1(chars('0', '9'))) ++
    rule("Num", "Digits~", c => c.text("Digits")) ++
    rule("LNum", "Digits {Digits.rightMostLast - Digits.leftMost + 1}", c => c.text("Digits")) ++
    rule("Num1", "Digits {1}", c => c.text("Digits")) ++
    rule("Num2", "Digits {2}", c => c.text("Digits")) ++
    rule("S", "Num(2)", c => c.Num[String]) ++
    rule("L", "LNum_1 LNum_2", Eq(Result("LNum_1"), Result("LNum_2"), 0), c => (c.LNum_1[String], c.LNum_2[String])) ++
    rule("Num12", "Num1 {Num1.val}", _.Num1[String]) ++
    rule("Num12", "Num2 {Num2.val}", _.Num2[String]) ++
    rule("A", "Num12", Eq(Result("Num12"), Zero, 2), _.Num12[String])

  val parser = Parser(grammar)
  def parse(nonterminal : String, s : String) = parser.parse[String](nonterminal, s)

  property("indent0") = parse("S", "10") == None
  property("indent1") = parse("S", " 10") == None
  property("indent2") = parse("S", "  10") == Some("10")

  property("result1") = parse("L", "1 2") == Some(("1", "2"))
  property("result2") = parse("L", "12 34") == Some(("12", "34"))
  property("result3") = parse("L", "123 456") == Some(("123", "456"))
  property("result4") = parse("L", "123 45") == None
  property("result5") = parse("A", "007") == None

}