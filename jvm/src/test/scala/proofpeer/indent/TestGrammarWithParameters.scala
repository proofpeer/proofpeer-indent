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
    rule("Num12", "Num1 {Num1.val}", c => c.Num1[String] + "(1)") ++
    rule("Num12", "Num2 {Num2.val}", c => c.Num2[String] + "(2)") ++
    rule("N", "Num12", _.Num12[String]) ++
    rule("N1", "Num12", Eq(Result("Num12"), Zero, 1), _.Num12[String]) ++
    rule("N2", "Num12", Eq(Result("Num12"), Zero, 2), _.Num12[String]) ++
    rule("C", "Digits {0}", c => "") ++
    rule("C", "C_1 {C_1.val + 1}", Leq(Result("C_1"), Zero, 4), c => c.C_1[String] + "x") ++
    rule("C3", "C", Eq(Result("C"), Zero, 3), _.C[String]) ++
    rule("C5", "C", Eq(Result("C"), Zero, 5), _.C[String]) ++
    rule("C6", "C", Eq(Result("C"), Zero, 6), _.C[String]) ++
    rule("Q", "Digits_1 Digits_2", Eq(Result("Digits_2"), Zero, 0), c => "") ++
    rule("X", "Digits {X.leftMost}", Eq(Result("X"), LeftMost("X"), 0), c => "")


  val parser = Parser(grammar)
  def parse(nonterminal : String, s : String) = parser.parse[String](nonterminal, s)

  property("indent0") = parse("S", "10") == None
  property("indent1") = parse("S", " 10") == None
  property("indent2") = parse("S", "  10") == Some("10")

  property("result1") = parse("L", "1 2") == Some(("1", "2"))
  property("result2") = parse("L", "12 34") == Some(("12", "34"))
  property("result3") = parse("L", "123 456") == Some(("123", "456"))
  property("result4") = parse("L", "123 45") == None
  property("result5") = parse("N", "007") == None
  property("result6") = parse("N1", "007") == Some("007(1)")
  property("result7") = parse("N2", "007") == Some("007(2)")

  property("cyclicresult1") = parse("C", "123") == None
  property("cyclicresult2") = parse("C3", "123") == Some("xxx")
  property("cyclicresult3") = parse("C5", "123") == Some("xxxxx")
  property("cyclicresult4") = parse("C6", "123") == None

  property("resultInConstraint1") = parse("Q", "1 2") == Some("")
  property("resultInConstraint2") = parse("X", "1") == Some("")




}