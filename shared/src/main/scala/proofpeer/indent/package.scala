package proofpeer

package object indent {

  import scala.language.implicitConversions
  import proofpeer.general.StringUtils._

  implicit def name2IndexedSymbol(name: String): IndexedSymbol = {
    GrammarSyntax.string2IndexedSymbol(name)
  }

  def string2rhs(s : String) : (Vector[IndexedSymbol], Vector[ParseParam]) = {
    GrammarSyntax.parseSymbols(s) match {
      case None => throw new RuntimeException("Cannot parse right hand side: '" + s + "'")
      case Some(symbols) =>
        val indexedSymbols = symbols.map(s => s._1)
        val params = symbols.map(s => s._2)
        (indexedSymbols, params)
    }
  }

  final class Parser(grammar : Grammar, printErrors : Boolean) {
    if (!grammar.isWellformed) {
      if (printErrors) {
        val errors = grammar.errors
        println("The grammar given to the parser has " + errors.size + " errors:")
        for (i <- 1 to errors.size) {
          println("  " + i + ". " + errors(i - 1))
        }
        println("")
      }
      throw new RuntimeException("grammar is not wellformed, cannot create parser")  
    }

    val earleyAutomaton = new indent.earley.EarleyAutomaton(grammar)
    
    def earley : indent.earley.Earley = new indent.earley.Earley(earleyAutomaton)
    
    def parse[T](nonterminal : String, text : String) : Option[T] = {
      earley.parse(Document.fromString(text), nonterminal) match {
        case Left(parsetree) =>
          if (parsetree.hasAmbiguities) None else Some(parsetree.getValue[T])
        case Right(errorposition) =>
          None
      }      
    }

    def parseAsTree(nonterminal : String, text : String) : Option[(Document, ParseTree)] = {
      val d = Document.fromString(text)
      earley.parse(d, nonterminal) match {
        case Left(parsetree) => Some((d, parsetree))
        case Right(errorposition) => None
      }            
    }
  }

  object Parser {
    def apply(grammar : Grammar, printErrors : Boolean = true) : Parser = {
      new Parser(grammar, printErrors)
    }
  }

  def lexrule(terminal : String, lexer : Lexer, priority : Option[Int] = None, scope : String = "") : Grammar = {
    Grammar(ScanRule(terminal, scope, priority, lexer))
  }

  def rule(terminal : String, rhs : regex.RegularExpr, priority : Option[Int] = None, scope : String = "") : Grammar = {
    Grammar(ScanRule(terminal, scope, priority, Lexer.untilWhitespace(rhs)))
  }

  def rule(nonterminal : String, rhs : String, action : ParseContext => Any) : Grammar = {
    val (r, params) = string2rhs(rhs)
    Grammar(ParseRule(nonterminal, r, params, Constraint.unconstrained, action))
  }

  def rule(nonterminal : String, rhs : String, constraint : Constraint, action : ParseContext => Any) : Grammar = {
    val (r, params) = string2rhs(rhs)
    Grammar(ParseRule(nonterminal, r, params, constraint, action))    
  }

}