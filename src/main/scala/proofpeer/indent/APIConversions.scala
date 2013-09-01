package proofpeer.indent

import API._
import scala.language.implicitConversions
import proofpeer.scala.lang._

object APIConversions {
     
  def name2SymbolName(name: String): SymbolName = {
    val all_letters = name.forall(isLetter)
    val all_digits = name.forall(isDigit)
    if (name == "" || !(all_letters || all_digits))
      throw new RuntimeException("Cannot convert name '" + name +"' to SymbolName")
    if (all_letters)
      SymbolNameStr(name)
    else
      SymbolNameCode(name.toInt)
  }
  
  def name2Symbol(name: String): Symbol = {
    if (name == "-") return TerminalRange(Range.universal)
    val u = name.indexOf("-")
    if (u < 0) {
      val symbolname = name2SymbolName(name)
      if (isUpperLetter(name.charAt(0)))
        Nonterminal(symbolname)
      else 
        Terminal(symbolname)
    } else {
      val left = name2SymbolName(name.substring(0, u))
      val right = name2SymbolName(name.substring(u+1))
      (left, right) match {
        case (SymbolNameCode(left), SymbolNameCode(right)) =>
          TerminalRange(Range.interval(left, right))
        case _ => 
          throw new RuntimeException("Invalid terminal range '" + name + "'")
      }
    }
  }

  implicit def name2Nonterminal(name: String): Nonterminal = {
    name2Symbol(name) match {
      case nonterminal: Nonterminal => nonterminal
      case x => throw new RuntimeException("nonterminal expected, found: "+x)
    }
  }

  implicit def name2Terminal(name: String): Terminal = {
    name2Symbol(name) match {
      case terminal: Terminal => terminal
      case x => throw new RuntimeException("terminal expected, found: "+x)
    }
  }

  implicit def name2IndexedSymbol(name: String): IndexedSymbol = {
    val u = name.indexOf("_")
    if (u >= 0) {
      val left = name.substring(0, u)
      val right = name.substring(u + 1)
      if (left == "" || right == "" ||
        !left.forall(isLetter(_)) || !right.forall(isDigit(_)))
        throw new RuntimeException("Cannot convert name '" + name + "' to IndexedSymbol")
      IndexedSymbol(name2Symbol(left), Some(right))
    } else {
      IndexedSymbol(name2Symbol(name), None)
    }
  }
  
  def string2rhs(s : String) : Vector[IndexedSymbol] = {
    s.split(" ").map(name2IndexedSymbol(_)).toVector
  }
    
}