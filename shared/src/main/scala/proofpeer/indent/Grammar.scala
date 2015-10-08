package proofpeer.indent

import scala.language.dynamics

/** IndexedSymbols are symbols that carry an optional index tag.
  * The tags can be used to distinguish otherwise equal symbols on the right hand side
  * of a rule so that they can be referenced in the layout constraint and the parse action.
  */
case class IndexedSymbol(symbol : String, index : Option[String]) {
  override def toString() : String = {
    index match {
      case None => symbol 
      case Some(index) => symbol + "_" + index
    }
  }
}

sealed trait Rule {
  def symbol : String
}


case class ScanRule(symbol : String, scope : String, priority : Option[Int], lexer : Lexer) extends Rule

trait ParseContext extends Dynamic {
  def result(indexedSymbol : IndexedSymbol) : ParseTree
  def document : Document
  def grammar : Grammar
  def rule : ParseRule
  def span : Span // null means empty span
  def startPosition : Int // the start position in the document (inclusive)
  def endPosition : Int // the end position in the document (exclusive)

  // convencience methods
  
  def text(indexedSymbol : IndexedSymbol) : String = {
    document.getText(result(indexedSymbol).span)
  }

  def text : String = {
    document.getText(span)
  }

  def span(indexedSymbol : IndexedSymbol) : Span = result(indexedSymbol).span

  def selectDynamic[T](s : String) : T = result(s).getValue[T]

}

case class ParseRule(symbol : String, rhs : Vector[IndexedSymbol],  
  params : Vector[ParseParam], constraint : Constraint, result : ParseParam,
  action : ParseContext => Any) extends Rule

trait AmbiguityResolution {
  def computeValue(nonterminal : String, span : Span, alternatives : Vector[ParseTree]) : Any
}

sealed trait GrammarError 

object GrammarError {
  
  case class MultipleScanrules(symbol : String, rules : List[Int]) extends GrammarError {
    override def toString : String = "Terminal '" + symbol +"' has " + rules.size + " scanrules associated with it."
  }

  case class NonterminalIsTerminal(symbol : String, parserules : List[Int], scanrules : List[Int]) extends GrammarError {
    override def toString : String = "Symbol '" + symbol +"' is used both as nonterminal and as terminal."
  }

  case class UnknownSymbol(unknownSymbol : String, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + unknownSymbol + "' used in the definition of '" + symbol +"' is unknown."
  }

  case class UnknownLayoutSymbol(unknownSymbol : IndexedSymbol, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + unknownSymbol + "' is referenced in the constraints / parameters but doesn't appear in the definition of '" + symbol + "'."    
  }

  case class AmbiguousLayoutSymbol(ambiguousSymbol : IndexedSymbol, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + ambiguousSymbol + "' is referenced in the constraints / parameters but is ambiguous in the definition of '" + symbol + "'."    
  }

  case class UnavailableLayoutSymbol(ambiguousSymbol : IndexedSymbol, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + ambiguousSymbol + "' is referenced in one of the parameters but is not available at that point in the definition of '" + symbol + "'."    
  }

  case class UnexpectedResult(symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + symbol + "' is nullable, but one of its nullable rules has a result."
  }

}

class Grammar(val rules : Vector[Rule], val ambiguityResolution : Option[AmbiguityResolution])
{

    def ++ (other : Grammar) : Grammar = {
      val r = 
        (ambiguityResolution, other.ambiguityResolution) match {
          case (None, r) => r
          case (r, None) => r
          case (u, v) if u == v => u
          case _ => throw new RuntimeException("incompatible ambiguity resolutions")
        }
      new Grammar(rules ++ other.rules, r)
    }

    private def check() : Vector[GrammarError] = {
      import GrammarError._
      var errors : Vector[GrammarError] = Vector()
      var scanSymbols : Map[String, List[Int]] = Map()
      var parseSymbols : Map[String, List[Int]] = Map()
      var ruleindex = 0
      for (rule <- rules) {
        rule match {
          case rule : ParseRule =>
            parseSymbols.get(rule.symbol) match {
              case None => parseSymbols += (rule.symbol -> List(ruleindex))
              case Some(indices) => parseSymbols += (rule.symbol -> (indices :+ ruleindex))
            }
            val symbols = Constraint.collectSymbols(rule.constraint) ++ 
              ParseParam.collectSymbols(rule.params) ++ ParseParam.collectSymbols(rule.result)
            val indexedSymbol = IndexedSymbol(rule.symbol, None)
            val symbolsOfRule = rule.rhs :+ indexedSymbol
            val frequency = symbolsOfRule.groupBy(l => l).map(t => (t._1, t._2.size))
            for (symbol <- symbols) {
              frequency.get(symbol) match {
                case None => errors :+= UnknownLayoutSymbol(symbol, rule.symbol, ruleindex)
                case Some(f) =>
                  if (f != 1) errors :+= AmbiguousLayoutSymbol(symbol, rule.symbol, ruleindex)
              }
            }

            var paramIndex = 0
            var parsedSymbols : Set[IndexedSymbol] = Set()
            while (paramIndex < rule.params.size) {
              val usedSymbols = ParseParam.collectSymbols(rule.params(paramIndex))
              if (!(usedSymbols subsetOf parsedSymbols)) {
                for (s <- usedSymbols -- parsedSymbols) 
                  errors :+= UnavailableLayoutSymbol(s, rule.symbol, ruleindex)
              }
              parsedSymbols = parsedSymbols + rule.rhs(paramIndex)
              paramIndex = paramIndex + 1
            }
            val usedSymbols = ParseParam.collectSymbols(rule.result)
            if (!(usedSymbols subsetOf parsedSymbols)) {
              for (s <- usedSymbols -- parsedSymbols)
                errors :+= UnavailableLayoutSymbol(s, rule.symbol, ruleindex)
            }
          case rule : ScanRule =>
            scanSymbols.get(rule.symbol) match {
              case None => scanSymbols += (rule.symbol -> List(ruleindex))
              case Some(indices) => scanSymbols += (rule.symbol -> (indices :+ ruleindex))
            }
        }
        ruleindex += 1
      }
      for ((symbol, indices) <- scanSymbols) {
        if (indices.size > 1) errors :+= MultipleScanrules(symbol, indices)
        parseSymbols.get(symbol) match {
          case None =>
          case Some(rules) =>
            errors :+= NonterminalIsTerminal(symbol, rules, indices)
        }
      }
      ruleindex = 0
      val nullables = nullableNonterminals
      for (rule <- rules) {
        rule match {
          case rule : ScanRule =>
          case rule : ParseRule =>
            var nullable : Boolean = true
            for (indexedSymbol <- rule.rhs) {
              if (!scanSymbols.get(indexedSymbol.symbol).isDefined
                  && !parseSymbols.get(indexedSymbol.symbol).isDefined)
              {
                errors :+= UnknownSymbol(indexedSymbol.symbol, rule.symbol, ruleindex)
              }
              if (!nullables.contains(indexedSymbol.symbol)) nullable = false
            }
            if (nullable && rule.result != ParseParam.Const(earley.Earley.DEFAULT_RESULT)) 
              errors :+= UnexpectedResult(rule.symbol, ruleindex)
            parseSymbols.get(rule.symbol) match {
              case None => parseSymbols += (rule.symbol -> List(ruleindex))
              case Some(indices) => parseSymbols += (rule.symbol -> (indices :+ ruleindex))
            }
        }
        ruleindex += 1
      }
      errors
    }

    private def computeScanRules : Map[String, ScanRule] = {
      var srules : Map[String, ScanRule] = Map()
      for (r <- rules) {
        r match {
          case r : ScanRule => srules += (r.symbol -> r)
          case _ =>
        }
      }
      srules
    }

    private def computeParseRules : (Map[String, Vector[ParseRule]], Set[String]) = {
      var prules : Map[String, Vector[ParseRule]] = Map()
      var symbols : Set[String] = Set()
      for (r <- rules) {
        r match {
          case r : ParseRule =>
            for (indexedSymbol <- r.rhs) symbols += indexedSymbol.symbol
            prules.get(r.symbol) match {
              case None => prules += (r.symbol -> Vector(r))
              case Some(rules) => prules += (r.symbol -> (rules :+ r))
            }
          case _ =>
        }
      }
      (prules, symbols)
    }

    lazy val scanrules = computeScanRules

    lazy val (parserules, usedSymbols) = computeParseRules

    lazy val terminals : Set[String] = scanrules.keys.toSet.intersect(usedSymbols)

    lazy val nonterminals : Set[String] = parserules.keys.toSet

    lazy val errors = check()

    lazy val nullableNonterminals : Set[String] = {
      var nullable : Set[String] = Set()
      var changed : Boolean = false
      do {
        changed = false
        for ((nonterminal, rules) <- parserules) {
          if (!nullable.contains(nonterminal)) {
            for (rule <- rules) {
              if (rule.rhs.forall(s => nullable.contains(s.symbol)))
              {
                nullable += nonterminal
                changed = true
              }
            }
          }
        }
      } while (changed)
      nullable
    }

    private def computeRhsIndices(nonterminal : String, ruleindex : Int) : Map[IndexedSymbol, Int] = {
      val rule = parserules(nonterminal)(ruleindex)
      var m : Map[IndexedSymbol, Int] = Map()
      var i = rule.rhs.size
      for (symbol <- rule.rhs.reverse) {
        i -= 1
        m += (symbol -> i)
      }
      m
    }

    private lazy val computedRhsIndices : Map[(String, Int), Map[IndexedSymbol, Int]] = {
      var m : Map[(String, Int), Map[IndexedSymbol, Int]] = Map()
      for ((nonterminal, rules) <- parserules) {
        for (i <- 0 until rules.size) m += ((nonterminal, i) -> computeRhsIndices(nonterminal, i))
      }
      m
    }

    def rhsIndices(nonterminal : String, ruleindex : Int) : Map[IndexedSymbol, Int] = computedRhsIndices((nonterminal, ruleindex))

    def isWellformed : Boolean = errors.isEmpty
}

object Grammar {
  def apply(rules : Rule*) : Grammar = new Grammar(rules.toVector, None)
  def apply(ambiguityResolution : AmbiguityResolution) : Grammar = new Grammar(Vector(), Some(ambiguityResolution))
  def apply(r : (String, Span, Vector[ParseTree]) => Any) : Grammar = 
    apply(
      new AmbiguityResolution { 
        def computeValue(nonterminal : String, span : Span, trees : Vector[ParseTree]) : Any = 
          r(nonterminal, span, trees)
      })
}

