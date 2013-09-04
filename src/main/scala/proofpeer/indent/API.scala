package proofpeer.indent

import scala.collection.immutable._

/** The main entry point for the ProofPeer Indent API. 
  *  All main functionality of Indent is available through this API object.
  */
object API {
  
  /** The name of a terminal or nonterminal symbol. */
  sealed abstract class SymbolName
  
  /** The symbol name is a string. */
  case class SymbolNameStr(s : String) extends SymbolName {
    override def toString() = s
  }
  
  /** The symbol name is an integer. */
  case class SymbolNameCode(code : Int) extends SymbolName {
    override def toString() = code.toString
  }

  /** Symbols can be nonterminals, terminals, or terminal ranges. */
  sealed abstract class Symbol() {
    def isNonterminal : Boolean
  }
  
  /** Nonterminal symbols. 
    * By convention the name consists only of letters, where the first letter is upper case.
    */
  case class Nonterminal(name : SymbolName) extends Symbol {
    override def toString() = name.toString
    def isNonterminal = true
  }
  
  /** Terminal symbols.
    * By convention the name either consists only of letters, 
    * where the first letter is lower case, or otherwise is a [[SymbolNameCode]].    
    */
  case class Terminal(name : SymbolName) extends Symbol {
    override def toString() = name.toString
    def isNonterminal = false
  }
  
  /** Terminal range symbols. 
    * Terminal ranges are used to denote compact sets of terminals.
    * For example, in the following, `terminal_range` could be used to denote a terminal range
    * of ASCII letters and digits:
    * {{{
    * val lowercase = Range.interval(0x41, 0x5a)
    * val uppercase = Range.interval(0x61, 0x7a)
    * val digits = Range.interval(0x30, 0x39)
    * val terminal_range = TerminalRange(Range.add(lowercase, uppercase, digits))
    * }}}
    */
  case class TerminalRange(range : Range) extends Symbol {
    def contains(name : SymbolName) : Boolean = {
      name match {
        case SymbolNameStr(_) => false
        case SymbolNameCode(code) => range.contains(code)
      }
    }
    override def toString() = range.toString()
    def isNonterminal = false
  }
  
  /** IndexedSymbols are symbols that carry an optional index tag.
    * The tags can be used to distinguish otherwise equal symbols on the right hand side
    * of a rule so that they can be referenced in [[Constraints]].
    */
  case class IndexedSymbol(symbol : Symbol, index : Option[String]) {
    override def toString() : String = {
      index match {
        case None => symbol.toString 
        case Some(index) => symbol.toString + "_" + index
      }
    }
  }
  
  /** Augments an IndexedSymbol with an annotation whether its layout is to be ignored. */
  case class AnnotatedSymbol(indexedSymbol : IndexedSymbol, ignore_layout : Boolean)
  
  import Constraints.Constraint
 
  /** A grammar rule with constraints. */
  case class Rule[S](lhs : Nonterminal, rhs : Vector[AnnotatedSymbol], 
                  constraint : Constraint[S])
  
  /** Compact specification of grammar rules.
    * Uses [[APIConversions.string2rhs]] to process the `rhs` parameter.
    */
  def rule(lhs : Nonterminal, rhs : String, constraint : Constraint[IndexedSymbol]) =
    Rule(lhs, APIConversions.string2rhs(rhs), constraint)

  /** Compact specification of grammar rules. */    
  def rule(lhs : Nonterminal, rhs : String) : Rule[IndexedSymbol] =
    rule(lhs, rhs, Constraints.Unconstrained)
    
  /** Grammar annotations. */
  sealed abstract class Annotation
  
  /** Grammar annotation to mark a nonterminal as lexical. */
  case class Lexical(nonterminal : Nonterminal) extends Annotation
  
  /** Grammar annotation to mark one lexical nonterminal as having less priority as another
    * lexical nonterminal.
    */
  case class LessPriority(a : Nonterminal, b : Nonterminal) extends Annotation
    
  /** A grammar consists of rules and annotations. */
  case class Grammar(rules : Vector[Rule[IndexedSymbol]], annotations : Vector[Annotation]) {
    lazy val info = GrammarChecker.check(this)
    lazy val wellformed = info.wellformed
  }
      
  def test () : String = {
    import APIConversions._
    import Constraints._
    val rules = Vector(
      rule("ST", "if E then ^ST_1 else ST_2", Align("if", "ST_1")),
      rule("A", ""),
      rule("B", "A A")
    )
    val annotations = Vector(
      Lexical("A"),
      Lexical("B"),
      LessPriority("A", "B"),
      Lexical("ST"),
      Lexical("E")
    )
    
    val grammar = Grammar(rules, annotations)
    var message = "wellformed: " + grammar.wellformed
        
    for (error <- grammar.info.errors) {
      message = message + "\nerror: " + error
    }
    message += "\n\nrules:\n"+grammar.info.rules
    
    val document = UnicodeDocument.fromString("hello\n\nworld, fellas :-)𝒫\uD835\uDCAB")

    message += "\n\ndocument size: " + document.size
    
    "<pre>\n" + message + "\n</pre>"
    
    "unicode:" + unicode()
        
  }
  
  def unicode() : String = {
    //val str = "A∀𝒫"
    val str = "A∀\uD835\uDCAB"  
    var s : String = ""
    for (c <- str) {
      val code : Int = c
      s = s + code + " "
    }
    s
  }
    
  def main(args : Array[String]) {
    println(test())    
  }
  
}