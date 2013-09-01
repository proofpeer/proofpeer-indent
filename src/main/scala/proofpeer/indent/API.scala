package proofpeer.indent

import scala.collection.immutable._

object API {

  sealed abstract class Symbol
  case class Nonterminal(name : String) extends Symbol
  case class Terminal(name : String) extends Symbol
  
  case class IndexedSymbol(symbol : Symbol, index : Option[String])
 
  case class Rule(lhs : Nonterminal, rhs : List[IndexedSymbol], constraint : Constraint)
  
  def rule(lhs : Nonterminal, rhs : String)(implicit constraint : Constraint) =
    Rule(lhs, APIConversions.string2rhs(rhs), constraint)
   
  sealed abstract class Annotation
  case class Lexical(nonterminal : Nonterminal) extends Annotation
  case class Less(a : Nonterminal, b : Nonterminal) extends Annotation
  
  case class Grammar(rules : List[Rule], annotations : List[Annotation]) extends Annotation
    
  def test () : String = {
    import APIConversions._
    import Constraints._
    val r = rule("ST", "if E then ST_1 else ST_2")
    "rule = " + r
  }
    
  def main(args : Array[String]) {
    println(test())    
  }
  
}