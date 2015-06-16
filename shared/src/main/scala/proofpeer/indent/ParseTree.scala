package proofpeer.indent

sealed trait ParseTree {
  def symbol : String
  def span : Span
  def hasAmbiguities : Boolean
  def ambiguities : List[AmbiguousNode]
  def getValue[T] : T
  def countTrees : Int
}

final case class AmbiguousNode(nonterminal : String, span : Span, alternatives : Vector[NonterminalNode]) extends ParseTree {
  def symbol = nonterminal
  def hasAmbiguities = true
  def ambiguities = List(this)
  def getValue[T] = throw new RuntimeException("AmbiguousNode (nonterminal="+nonterminal+",span="+span+") has no value")
  lazy val countTrees : Int = {
    var c : Int = 0
    for (alternative <- alternatives) c += alternative.countTrees
    c
  }
}

final case class NonterminalNode(nonterminal : String, ruleindex : Int, span : Span, rhs : Vector[ParseTree], value : Any) extends ParseTree {
  def symbol = nonterminal
  lazy val hasAmbiguities = rhs.exists(_.hasAmbiguities)
  def ambiguities = rhs.toList.flatMap(_.ambiguities)
  def getValue[T] = value.asInstanceOf[T]
  lazy val countTrees : Int = {
    var c : Int = 1
    for (tree <- rhs) c *= tree.countTrees
    c
  }
}

final case class TerminalNode(terminal : String, span : Span) extends ParseTree {
  def symbol = terminal
  def hasAmbiguities = false
  def ambiguities = List()
  def getValue[T] = throw new RuntimeException("TerminalNode has no value")
  def countTrees : Int = 1
}