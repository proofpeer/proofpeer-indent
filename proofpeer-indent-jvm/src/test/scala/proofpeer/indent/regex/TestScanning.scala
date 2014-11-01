package proofpeer.indent.regex

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TestScanning extends Properties("Scanning") {
  
  val digit = CHAR(Range(48, 57))
  val letter = CHAR(Range(65, 90) + Range(97, 122))
  val digits = REPEAT1(digit)
  val letters = REPEAT1(letter)
  val alphanumeric = REPEAT1(ALT(digit, letter))
  val alphanumerics = REPEAT1(ALT(digits, letters))
  val apostroph = CHAR(Range(39))
  val not_apostroph = CHAR(-Range(39))
  val apostroph_string = SEQ(apostroph, SEQ(REPEAT(not_apostroph), apostroph))

  def scan(regex : RegularExpr, s : String) : Boolean = {
    val nfa = NFA.fromRegularExpr(0, regex, 0)
    val dfa = DFA.fromNFA(nfa)
    val stream = CharacterStream.fromString(s)
    val (len, ids) = DFA.run(dfa, stream)
    len == s.size && ids == Set(0)
  }
 
  def scandebug(regex : RegularExpr, s : String) : Boolean = {
    println("-------------------------------")
    val nfa = NFA.fromRegularExpr(0, regex, 0)
    nfa.display()
    println("-------")
    val dfa = DFA.fromNFA(nfa)
    dfa.display()
    val stream = CharacterStream.fromString(s)
    val (len, ids) = DFA.run(dfa, stream)
    val result = len == s.size && ids == Set(0)
    println("-------------------------------")
    result
  }

  property("ex 1") = scan(digit, "7") 
  property("ex 2") = !scan(digit, "a")
  property("ex 3") = !scan(letter, "7") 
  property("ex 4") = scan(letter, "a")
  property("digits 1") = !scan(digits, "")
  property("digits 2") = scan(digits, "489")
  property("digits 3") = !scan(digits, "abc")
  property("digits 4") = !scan(digits, "a12b3cccCCc4A")
  property("letters 1") = !scan(letters, "")
  property("letters 2") = !scan(letters, "489")
  property("letters 3") = scan(letters, "abc")
  property("letters 4") = !scan(letters, "a12b3cccCCc4A")
  property("alphanumeric 1") = !scan(alphanumeric, "")
  property("alphanumeric 2") = scan(alphanumeric, "489")
  property("alphanumeric 3") = scan(alphanumeric, "abc")
  property("alphanumeric 4") = scan(alphanumeric, "a12b3cccCCc4A")
  property("alphanumeric 4") = !scan(alphanumeric, "a12b3c-ccCCc4A")  
  property("empty repeat") = scan(REPEAT(digit), "")
  property("alphanumerics 1") = !scan(alphanumerics, "")
  property("alphanumerics 2") = scan(alphanumerics, "489")
  property("alphanumerics 3") = scan(alphanumerics, "abc")
  property("alphanumerics 4") = scan(alphanumerics, "a12b3cccCCc4A")
  property("alphanumerics 4") = !scan(alphanumerics, "a12b3c-ccCCc4A")  
  property("string 1") = scan(apostroph_string, "''")
  property("string 2") = scan(apostroph_string, "'hello'")
  property("string 3") = !scan(apostroph_string, "'hello")
  property("string 4") = !scan(apostroph_string, "'hello't")



}
