package proofpeer.indent

import scala.collection.immutable._

object API {
  
  sealed abstract class SymbolName
  case class SymbolNameStr(s : String) extends SymbolName {
    override def toString() = s
  }
  case class SymbolNameCode(code : Int) extends SymbolName {
    override def toString() = code.toString
  }

  sealed abstract class Symbol() 
  case class Nonterminal(name : SymbolName) extends Symbol {
    override def toString() = name.toString
  }
  case class Terminal(name : SymbolName) extends Symbol {
    override def toString() = name.toString
  }
  case class TerminalRange(range : Range) extends Symbol {
    def contains(name : SymbolName) : Boolean = {
      name match {
        case SymbolNameStr(_) => false
        case SymbolNameCode(code) => range.contains(code)
      }
    }
    override def toString() = range.toString()
  }
  
  case class IndexedSymbol(symbol : Symbol, index : Option[String]) {
    override def toString() : String = {
      index match {
        case None => symbol.toString 
        case Some(index) => symbol.toString + "_" + index
      }
    }
  }
 
  case class Rule(lhs : Nonterminal, rhs : Vector[IndexedSymbol], constraint : Constraint)
  
  def rule(lhs : Nonterminal, rhs : String)(implicit constraint : Constraint) =
    Rule(lhs, APIConversions.string2rhs(rhs), constraint)
   
  sealed abstract class Annotation
  case class Lexical(nonterminal : Nonterminal) extends Annotation
  case class Less(a : Nonterminal, b : Nonterminal) extends Annotation
  
  case class Grammar(rules : Vector[Rule], annotations : Vector[Annotation]) extends Annotation
    
  def test () : String = {
    import APIConversions._
    import Constraints._
    val r = rule("ST", "if E then ST_1 else ST_2 otherwise 07160 - 10-34 ")
    "rule: " + r 
  }
    
  def main(args : Array[String]) {
    println(test())    
  }
  
}