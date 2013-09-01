package proofpeer.indent

sealed abstract class Constraint

object Constraints {
  
  type S = API.IndexedSymbol
      
  case class And(constraints : List[Constraint]) extends Constraint
  case class Or(constraints : List[Constraint]) extends Constraint
  case class Not(constraint : Constraint) extends Constraint
  case class Implies(assumption : Constraint, conclusion : Constraint) extends Constraint
  case class Layout(layout : LayoutConstraint) extends Constraint
  
  implicit def Unconstrained = And(List())
    
  sealed abstract class LayoutConstraint
  
  /** B follows A without any whitespace separating A and B */
  case class Connect(A : S, B : S) extends LayoutConstraint
  
  /** B starts in the same line that A ends in */
  case class Line(A : S, B : S) extends LayoutConstraint
  
  /** B is indented relative to A */
  case class Indent(A : S, B : S) extends LayoutConstraint  
  
  /** A and B align on the left hand side */
  case class Align(A : S, B : S) extends LayoutConstraint
  
  /** The first line of A is indented less than all other lines of A */
  case class Protrude(A : S) extends LayoutConstraint  
  
  /** There is no non-whitespace before A in the first line of A */
  case class First(A : S) extends LayoutConstraint
  
}