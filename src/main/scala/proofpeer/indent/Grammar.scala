package proofpeer.indent

import proofpeer.indent.regex.RegularExpr

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

case class ScanRule(symbol : String, scope : Int, priority : Option[Int], regex : RegularExpr) extends Rule

trait ParseContext {
  def apply[T](indexedSymbol : IndexedSymbol) : T
}

case class ParseRule(symbol : String, rhs : Vector[IndexedSymbol], constraint : Constraint, 
  action : ParseContext => Any) extends Rule

sealed trait GrammarError 

object GrammarError {
  
  case class MultipleScanrules(symbol : String, rules : List[Int]) extends GrammarError {
    override def toString : String = "Terminal '" + symbol +"' has " + rules.size + " scanrules associated with it."
  }

  case class ScanruleMatchesEmpty(symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "Terminal '" + symbol +"' matches the empty string."
  }

  case class NonterminalIsTerminal(symbol : String, parserules : List[Int], scanrules : List[Int]) extends GrammarError {
    override def toString : String = "Symbol '" + symbol +"' is used both as nonterminal and as terminal."
  }

  case class UnknownSymbol(unknownSymbol : String, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + unknownSymbol + "' used in the definition of '" + symbol +"' is unknown."
  }

  case class UnknownSymbolInConstraint(unknownSymbol : IndexedSymbol, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + unknownSymbol + "' is referenced in the constraints but doesn't appear in the corresponding right hand side in the definition of '" + symbol + "'."    
  }

  case class AmbiguousSymbolInConstraint(ambiguousSymbol : IndexedSymbol, symbol : String, rule : Int) extends GrammarError {
    override def toString : String = "The symbol '" + ambiguousSymbol + "' is referenced in the constraints but doesn't appear in the corresponding right hand side in the definition of '" + symbol + "'."    
  }

}

class Grammar(val rules : Vector[Rule])
{

    def ++ (other : Grammar) : Grammar = {
      Grammar(rules ++ other.rules)
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
            val symbols = Constraint.collectSymbols(rule.constraint)
            val frequency = rule.rhs.groupBy(l => l).map(t => (t._1, t._2.size))
            for (symbol <- symbols) {
              frequency.get(symbol) match {
                case None => errors :+= UnknownSymbolInConstraint(symbol, rule.symbol, ruleindex)
                case Some(f) =>
                  if (f != 1) errors :+= AmbiguousSymbolInConstraint(symbol, rule.symbol, ruleindex)
              }
            }
          case rule : ScanRule =>
            scanSymbols.get(rule.symbol) match {
              case None => scanSymbols += (rule.symbol -> List(ruleindex))
              case Some(indices) => scanSymbols += (rule.symbol -> (indices :+ ruleindex))
            }
            if (proofpeer.indent.regex.Utils.matchesEmpty(rule.regex))
              errors :+= ScanruleMatchesEmpty(rule.symbol, ruleindex)
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
      for (rule <- rules) {
        rule match {
          case rule : ScanRule =>
          case rule : ParseRule =>
            for (indexedSymbol <- rule.rhs) {
              if (!scanSymbols.get(indexedSymbol.symbol).isDefined
                  && !parseSymbols.get(indexedSymbol.symbol).isDefined)
              {
                errors :+= UnknownSymbol(indexedSymbol.symbol, rule.symbol, ruleindex)
              }
            }
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
              if (rule.rhs.forall(s => nullable.contains(s.symbol))) {
                nullable += nonterminal
                changed = true
              }
            }
          }
        }
      } while (changed)
      nullable
    }

    def isWellformed : Boolean = errors.isEmpty
}

object Grammar {
  def apply(rules : Vector[Rule]) : Grammar = new Grammar(rules)
}

object GrammarConversions {

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

}

