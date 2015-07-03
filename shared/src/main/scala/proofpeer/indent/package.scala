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

  private def string2rhs(s : String) : Vector[IndexedSymbol] = {
    if (s.trim().isEmpty())
      Vector()
    else
      split_nonempty(s, " ").map(name2IndexedSymbol(_)).toVector
  }

  def string2rhsi(s : String) : (Vector[IndexedSymbol], Vector[Boolean]) = {
    val u = s.indexOf("[")
    val v = s.indexOf("]")
    if (u < 0 && v < 0) {
      val rhs = string2rhs(s)
      (rhs, Vector.fill(rhs.size)(true))
    } else if (u >= 0 && v > u) {
      val t = s.substring(u + 1, v)
      val rhs = string2rhs(t)
      val (r, i) = string2rhsi(s.substring(v+1))
      (rhs ++ r, Vector.fill(rhs.size)(false) ++ i)
    } else throw new RuntimeException("] cannot come before [")
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

    val earleyAutomaton = new earley.EarleyAutomaton(grammar)
    val earleyParser = new earley.Earley(earleyAutomaton)
    
    def parse[T](nonterminal : String, text : String) : Option[T] = {
      earleyParser.parse(Document.fromString(text), nonterminal) match {
        case Left(parsetree) =>
          if (parsetree.hasAmbiguities) None else Some(parsetree.getValue[T])
        case Right(errorposition) =>
          None
      }      
    }

    def parseAsTree(nonterminal : String, text : String) : Option[(Document, ParseTree)] = {
      val d = Document.fromString(text)
      earleyParser.parse(d, nonterminal) match {
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

  def rule(terminal : String, rhs : regex.RegularExpr, priority : Option[Int] = None, scope : String = "") : Grammar = {
    Grammar(ScanRule(terminal, scope, priority, rhs))
  }

  def rule(nonterminal : String, rhs : String, action : ParseContext => Any) : Grammar = {
    val (r, i) = string2rhsi(rhs)
    Grammar(ParseRule(nonterminal, r, i, Constraint.unconstrained, action))
  }

  def rule(nonterminal : String, rhs : String, constraint : Constraint, action : ParseContext => Any) : Grammar = {
    val (r, i) = string2rhsi(rhs)
    Grammar(ParseRule(nonterminal, r, i, constraint, action))    
  }

 
}