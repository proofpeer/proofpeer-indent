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
}

object Grammar {
  def apply(rules : Vector[Rule]) : Grammar = new Grammar(rules)
}

