package proofpeer.indent

import proofpeer.indent.regex.RegularExpr
import proofpeer.indent.layout.Constraint

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

class Grammar(val rules : Vector[Rule])
{
    def ++(other : Grammar) : Grammar = {
      Grammar(rules ++ other.rules)
    }

    private def computeScanRules : Map[String, ScanRule] = {
      var srules : Map[String, ScanRule] = Map()
      for (r <- rules) {
        r match {
          case r : ScanRule =>
            if (srules.get(r.symbol).isDefined) 
              throw new RuntimeException("duplicate scanrule for " + r.symbol)
            srules += (r.symbol -> r)
          case _ =>
        }
      }
      srules
    }

    def computeParseRules : (Map[String, Vector[ParseRule]], Set[String]) = {
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

