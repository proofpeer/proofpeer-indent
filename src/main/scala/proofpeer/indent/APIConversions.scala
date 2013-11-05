package proofpeer.indent

import API._
import scala.language.implicitConversions
import proofpeer.scala.lang._

object APIConversions {
     
  def name2SymbolName(name: String): SymbolName = {
    val all_letters = name.forall(isASCIILetter)
    val all_digits = name.forall(isASCIIDigit)
    if (name == "" || !(all_letters || all_digits))
      throw new RuntimeException("Cannot convert name '" + name +"' to SymbolName")
    if (all_letters)
      SymbolNameStr(name)
    else
      SymbolNameCode(name.toInt)
  }
  
  def name2Symbol(name: String): Symbol = {
    if (name == "-") return TerminalRange(Range.universal)
    var u = name.indexOf("-")
    val v = name.indexOf("*")
    if (u < 0 && v < 0) {
      val symbolname = name2SymbolName(name)
      if (isASCIIUpperLetter(name.charAt(0)))
        Nonterminal(symbolname)
      else 
        Terminal(symbolname)
    } else {
      val outside = u < 0
      if (outside) u = v
      val left = name2SymbolName(name.substring(0, u))
      val right = name2SymbolName(name.substring(u+1))
      (left, right) match {
        case (SymbolNameCode(left), SymbolNameCode(right)) =>
          TerminalRange(if (outside) Range.outside_interval(left, right) else Range.interval(left, right))
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
      val left = name2Symbol(name.substring(0, u))
      val right = name.substring(u + 1)
      if (right == "" || !right.forall(isASCIIDigit(_)))
        throw new RuntimeException("Cannot convert name '" + name + "' to IndexedSymbol")
      IndexedSymbol(left, Some(right))
    } else 
      IndexedSymbol(name2Symbol(name), None)
  }
  
  implicit def name2AnnotatedSymbol(name: String): AnnotatedSymbol = {
    if (!name.isEmpty() && name.charAt(0) == '^')
      AnnotatedSymbol(name.substring(1), true)
    else
      AnnotatedSymbol(name, false)
  }
  
  def literal(l: String) : String = {
    var s = ""
    var i = 0
    val len = l.length()
    while (i < len) {
      val c = codePointAt(l, i)
      i = i + charCount(c)
      s = s + " " + c
    }
    s + " "
  }
  
  def except(c : String) : String = {
    import proofpeer.scala.lang._
    if (!c.isEmpty()) {
      val x = codePointAt(c, 0)
      if (c.length() == charCount(x)) {
        return " " + x + "*" + x + " "
      }
    }
    throw new RuntimeException("except can only be applied to single codepoints")
  }
  
  def string2rhs(s : String) : Vector[AnnotatedSymbol] = {
    if (s.trim().isEmpty())
      Vector()
    else
      split_nonempty(s, " ").map(name2AnnotatedSymbol(_)).toVector
  }
    
}