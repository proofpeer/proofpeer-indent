package proofpeer

package object indent {

  import scala.language.implicitConversions
  import proofpeer.general.StringUtils._

  implicit def name2IndexedSymbol(name: String): IndexedSymbol = {
    val u = name.indexOf("_")
    if (u >= 0) {
      val left = name.substring(0, u)
      val right = name.substring(u + 1)
      if (right == "" || !right.forall(isASCIIDigit(_)))
        throw new RuntimeException("Cannot convert name '" + name + "' to IndexedSymbol")
      IndexedSymbol(left, Some(right))
    } else 
      IndexedSymbol(name, None)
  }

  def string2rhs(s : String) : Vector[IndexedSymbol] = {
    if (s.trim().isEmpty())
      Vector()
    else
      split_nonempty(s, " ").map(name2IndexedSymbol(_)).toVector
  }

  final class Parser[T](grammar : Grammar, nonterminal : String, printErrors : Boolean) {
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

    val earleyAutomaton = new earley.EarleyAutomaton(grammar)
    val earleyParser = new earley.Earley(earleyAutomaton)
    
    def parse(text : String) : Option[T] = {
      earleyParser.parse(Document.fromString(text), nonterminal) match {
        case Left(parsetree) =>
          if (parsetree.hasAmbiguities) None else Some(parsetree.getValue[T])
        case Right(errorposition) =>
          None
      }      
    }
  }

  object Parser {
    def apply[T](grammar : Grammar, nonterminal : String, printErrors : Boolean = true) : Parser[T] = {
      new Parser[T](grammar, nonterminal, printErrors)
    }
  }

  def rule(terminal : String, rhs : regex.RegularExpr, priority : Option[Int] = None, scope : String = "") : Grammar = {
    Grammar(ScanRule(terminal, scope, priority, rhs))
  }

  def rule(nonterminal : String, rhs : String, action : ParseContext => Any) : Grammar = {
    Grammar(ParseRule(nonterminal, string2rhs(rhs), Constraint.unconstrained, action))
  }

  def rule(nonterminal : String, rhs : String, constraint : Constraint, action : ParseContext => Any) : Grammar = {
    Grammar(ParseRule(nonterminal, string2rhs(rhs), constraint, action))    
  }

 
}