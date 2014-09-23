package proofpeer.indent

import scala.collection.immutable._

/*case class Dummy(nonterminal : API.Nonterminal, ruleindex : Int, dot : Int, origin : Int) {
  //def inc : DummyItem = Item(nonterminal, ruleindex, dot + 1, origin)
  //override def hashCode : Int = List
}*/


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
  
  trait TerminalLike {
    def isLike(name : Terminal) : Boolean
    def isNonterminal = false
  }
  
  /** Terminal symbols.
    * By convention the name either consists only of letters, 
    * where the first letter is lower case, or otherwise is a [[SymbolNameCode]].    
    */
  case class Terminal(name : SymbolName) extends Symbol with TerminalLike {
    override def toString() = name.toString
    def isLike(t : Terminal) = t.name == name
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
  case class TerminalRange(range : Range) extends Symbol with TerminalLike {
    def isLike(t : Terminal) : Boolean = {
      t.name match {
        case SymbolNameStr(_) => false
        case SymbolNameCode(code) => range.contains(code)
      }
    }
    override def toString() = range.toString()
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
  case class AnnotatedSymbol(indexedSymbol : IndexedSymbol, ignore_layout : Boolean) {
    override def toString : String = 
      if (ignore_layout) "^" + indexedSymbol else indexedSymbol.toString
  }
    
  type ParseAction = Derivation.Context => Any 
  
  val defaultParseAction : ParseAction = (c => null)

  import Constraints.Constraint
  
  /** A grammar rule with constraints. */
  case class Rule[S](lhs : Nonterminal, rhs : Vector[AnnotatedSymbol], 
                  constraint : Constraint[S], action : ParseAction)
      
  /** Grammar annotations. */
  sealed abstract class Annotation
  
  case class LexicalPriority(scope : Int, priority : Option[Int])

  /** Grammar annotation to mark a nonterminal as lexical. */
  case class Lexical(nonterminal : Nonterminal, priority : LexicalPriority) extends Annotation
      
  /** A grammar consists of rules and annotations. */
  case class Grammar(rules : Vector[Rule[IndexedSymbol]], annotations : Vector[Annotation]) {
    lazy val info = GrammarChecker.check(this)
    lazy val wellformed = info.wellformed
    //private lazy val bb_grammar = new proofpeer.inden
    lazy val parser : Parser = 
      proofpeer.indent.earley.Adapter.parser(this)
    lazy val recognizer : Recognizer = 
      proofpeer.indent.earley.Recognizer.parser(this)      
    def ++(other : Grammar) : Grammar = {
      Grammar(rules ++ other.rules, annotations ++ other.annotations)
    }
  }
  
  trait Parser {
    def grammar : Grammar
    def parse(document : Document, start : Nonterminal, k : Int) : Option[(Derivation.Value, Int)] 
    def parse(document : String, start : Nonterminal) : Option[Derivation.Value] = {
      val d = UnicodeDocument.fromString(document)
      parse(d, start, 0) match {
        case None => None
        case Some((v, i)) => if (i == d.size) Some(v) else None
      }
    }
  }

  trait Recognizer {
    def grammar : Grammar
    def parse(document : Document, start : Nonterminal, k : Int) : Option[(proofpeer.indent.earley.Recognizer.Value, Int)] 
    def parse(document : String, start : Nonterminal) : Option[proofpeer.indent.earley.Recognizer.Value] = {
      val d = UnicodeDocument.fromString(document)
      parse(d, start, 0) match {
        case None => None
        case Some((v, i)) => if (i == d.size) Some(v) else None
      }
    }
  }


  /** Compact specification of grammars.
    * Uses [[APIConversions.string2rhs]] to process the `rhs` parameter.
    */
  def rule(lhs : Nonterminal, rhs : String, constraint : Constraint[IndexedSymbol], action : ParseAction) : Grammar =
    Grammar(Vector(Rule(lhs, APIConversions.string2rhs(rhs), constraint, action)), Vector())

  def rule(lhs : Nonterminal, rhs : String, constraint : Constraint[IndexedSymbol]) : Grammar =
    rule(lhs, rhs, constraint, defaultParseAction)
    
  def rule(lhs : Nonterminal, rhs : String, action : ParseAction) : Grammar =
    rule(lhs, rhs, Constraints.Unconstrained[IndexedSymbol], action)

  def rule(lhs : Nonterminal, rhs : String) : Grammar =
    rule(lhs, rhs, Constraints.Unconstrained[IndexedSymbol])
    
  def tokenrule(lhs : Nonterminal, range : Range) : Grammar = {
    val symbol = AnnotatedSymbol(IndexedSymbol(TerminalRange(range), None), false)
    Grammar(Vector(Rule(lhs, Vector(symbol), Constraints.Unconstrained, defaultParseAction)), Vector())
  }
       
  /** Compact specification of grammars. */        
  def lexical(nonterminal : Nonterminal, prio : LexicalPriority) : Grammar =
    Grammar(Vector(), Vector(Lexical(nonterminal, prio)))
  
  def lexrule(lhs : Nonterminal, rhs : String, constraint : Constraint[IndexedSymbol], prio : LexicalPriority) : Grammar = {
    lexrule(lhs, rhs, constraint, defaultParseAction, prio)
  }
  
  def lexrule(lhs : Nonterminal, rhs : String, constraint : Constraint[IndexedSymbol], action : ParseAction, 
    prio : LexicalPriority) : Grammar = 
  {
    val symbols = APIConversions.string2rhs(rhs)
    var constraints = List(constraint)
    for (i <- 1 to (symbols.size - 1)) {
      val a = symbols(i-1).indexedSymbol
      val b = symbols(i).indexedSymbol
      constraints = (Constraints.Connect(a, b)) :: constraints
    }
    Grammar(Vector(Rule(lhs, symbols, Constraints.And(constraints), action)),
      Vector(Lexical(lhs, prio)))
  }
    
  def lexrule(lhs : Nonterminal, rhs : String, action : ParseAction, prio : LexicalPriority) : Grammar =
    lexrule(lhs, rhs, Constraints.Unconstrained, action, prio)

  def lexrule(lhs : Nonterminal, rhs : String, prio : LexicalPriority) : Grammar = 
    lexrule(lhs, rhs, Constraints.Unconstrained[IndexedSymbol], prio)
      
  def example1 : Grammar = {
    import APIConversions._
    import Constraints._
    val grammar =
      rule("ST", "if E then ^ST_1 else ST_2", Align("if", "ST_1")) ++
      rule("A", "") ++
      rule("B", "A A") ++
      lexical("A", LexicalPriority(0, None)) ++
      lexical("B", LexicalPriority(0, None)) ++
      lexical("ST", LexicalPriority(0, None)) ++
      lexical("E", LexicalPriority(0, None))
    grammar  
  }
  
  def reportGrammar(grammar : Grammar) = {
    var message = "wellformed: " + grammar.wellformed
        
    for (error <- grammar.info.errors) {
      message = message + "\nerror: " + error
    }
    message += "\n\nrules:\n"+grammar.info.rules
    
    message    
  }
    
  
}