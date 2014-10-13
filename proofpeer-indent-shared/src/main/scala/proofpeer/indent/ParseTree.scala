package proofpeer.indent

sealed trait ParseTree {
  def symbol : String
  def span : Span
  def hasAmbiguities : Boolean
  def ambiguities : List[AmbiguousNode]
  def getValue[T] : T
}

final case class AmbiguousNode(nonterminal : String, span : Span) extends ParseTree {
  def symbol = nonterminal
  def hasAmbiguities = true
  def ambiguities = List(this)
  def getValue[T] = throw new RuntimeException("AmbiguousNode has no value")
}

final case class NonterminalNode(nonterminal : String, ruleindex : Int, span : Span, rhs : Vector[ParseTree], value : Any) extends ParseTree {
  def symbol = nonterminal
  lazy val hasAmbiguities = rhs.exists(_.hasAmbiguities)
  def ambiguities = rhs.toList.flatMap(_.ambiguities)
  def getValue[T] = value.asInstanceOf[T]
}

final case class TerminalNode(terminal : String, span : Span) extends ParseTree {
  def symbol = terminal
  def hasAmbiguities = false
  def ambiguities = List()
  def getValue[T] = throw new RuntimeException("TerminalNode has no value")
}