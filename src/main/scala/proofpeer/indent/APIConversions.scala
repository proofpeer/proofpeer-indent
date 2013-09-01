package proofpeer.indent

import API._
import scala.language.implicitConversions
import proofpeer.scala.lang._

object APIConversions {
  
    
  def name2Symbol(name: String): Symbol = {
    if (name == "" || !name.forall(isLetter))
      throw new RuntimeException("Cannot convert name '" + name + "' to Symbol")
    if (isLowerLetter(name.charAt(0))) Terminal(name) else Nonterminal(name)
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
  
  def string2rhs(s : String) : List[IndexedSymbol] = {
    s.split(" ").map(name2IndexedSymbol(_)).toList
  }
    
}