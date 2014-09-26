package proofpeer.indent.layout

import proofpeer.indent.Grammar

object CollectQualifiers {

  def collect(constraint : Constraint) : Set[LayoutQualifier] = {
    constraint match {
      case And(constraints) => collect(constraints)
      case Or(constraints) => collect(constraints)
      case Not(constraint) => collect(constraint)
      case Implies(assumption, conclusion) => collect(List(assumption, conclusion))
      case Less((left, _), (right, _), _) => Set(left, right)
      case Leq((left, _), (right, _), _) => Set(left, right)
      case Eq((left, _), (right, _), _) => Set(left, right)
    }
  }

  def collect(constraints: List[Constraint]) : Set[LayoutQualifier] = {
    var qualifiers : Set[LayoutQualifier] = Set()
    for (constraint <- constraints) {
      qualifiers ++= collect(constraint)
    }
    qualifiers
  }

  def collect(grammar : Grammar) : Set[LayoutQualifier] = {
    var qualifiers : Set[LayoutQualifier] = Set()
    for ((_, rules) <- grammar.parserules) {
      val constraints = rules.toList.map(_.constraint)
      qualifiers ++= collect(constraints)
    }
    qualifiers
  }

}