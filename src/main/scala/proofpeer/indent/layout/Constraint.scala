package proofpeer.indent

package object layout {

  sealed abstract class Constraint
  case class And(constraints : List[Constraint]) extends Constraint
  case class Or(constraints : List[Constraint]) extends Constraint
  case class Not(constraint : Constraint) extends Constraint
  case class Implies(assumption : Constraint, conclusion : Constraint) 
    extends Constraint

  def and(constraints : Constraint*) : Constraint  = 
    And(constraints.toList)
    
  def or(constraints : Constraint*) : Constraint  = 
    Or(constraints.toList)  
    
  def not(constraint : Constraint) : Constraint  = 
    Not(constraint)
    
  def implies(asm : Constraint, concl : Constraint) 
        : Constraint  =
    Implies(asm, concl)      
    
  def ifThenElse(asm : Constraint, thenConcl : Constraint, 
        elseConcl : Constraint) : Constraint  =
    and(implies(asm, thenConcl), implies(not(asm), elseConcl))          

  def unconstrained : Constraint = And(List())
    
  type LayoutEntity = (LayoutQualifier, IndexedSymbol)

  /** left < right + delta */
  case class Less(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** left <= right + delta */
  case class Leq(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** left = right + delta */
  case class Eq(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** Denotes integer typed properties of the geometry of s */
  sealed abstract class LayoutQualifier {
    def apply(indexedSymbol : IndexedSymbol) : LayoutEntity = (this, indexedSymbol)
  }

  /** Row of the first line of s. */
  case object FirstRow extends LayoutQualifier

  /** Row of the last line of s. */
  case object LastRow extends LayoutQualifier

  /** Column of first non-whitespace character in the first line of s. */ 
  case object LeftMostInFirst extends LayoutQualifier

  /** Minimum column in which a character of s appears. */
  case object LeftMost extends LayoutQualifier

  /** Minimum column in which a character of s appears which is also in the first line of s. */
  case object LeftMostFirst extends LayoutQualifier

  /** Minimum column in which a character of s appears which is not in the first line of s.
    * This value is only defined if s consists of at least two rows. 
    */
  case object LeftMostRest extends LayoutQualifier

  /** Maximum column in which a character of s appears which is also in the last line of s. */
  case object RightMostLast extends LayoutQualifier

  /** B starts in the same line that A ends in. */
  def SameLine(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    and(Eq(LastRow(A), FirstRow(B), 0))
    
  /** A consists of a single line only. */
  def Line(A : IndexedSymbol) : Constraint  = 
    SameLine(A, A)    
    
  /** A and B both fit in the same single line. */
  def Line(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    Eq(FirstRow(A), LastRow(B), 0)

  /** B follows A without any whitespace separating A and B. */
  def Connect(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    and(SameLine(A, B), Eq(RightMostLast(A), LeftMostFirst(B), -1))

  /** There is no non-whitespace before A in the first line of A,
      and no other line of A is indented more than the first line. 
    */
  def First(A : IndexedSymbol) : Constraint  = 
    and(Eq(LeftMostInFirst(A), LeftMostFirst(A), 0),
        Leq(LeftMostFirst(A), LeftMostRest(A), 0))
    
  /** The first line of A is indented less than all other lines of A. */
  def Protrude(A : IndexedSymbol) : Constraint  = 
    or(Line(A), and(First(A), Less(LeftMostFirst(A), LeftMostRest(A), 0)))
    
  /** A and B are weakly aligned at their left border. */
  def WeakAlign(A : IndexedSymbol, B : IndexedSymbol) : Constraint  = 
    Eq(LeftMost(A), LeftMost(B), 0)

  /** B is weakly indented relative to A. */
  def WeakIndent(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    Less(LeftMost(A), LeftMost(B), 0)
       
  /** A and B are strongly aligned at their left border. */
  def Align(A : IndexedSymbol, B : IndexedSymbol) : Constraint  = 
    or(NullSpan(A), NullSpan(B), and(First(A), First(B), WeakAlign(A, B)))
    
  /** B is strongly indented relative to A. */
  def Indent(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    or(Eq(LastRow(A), LastRow(B), 0),
       and(First(A), WeakIndent(A, B), Eq(LeftMost(A), LeftMostFirst(A), 0)))

  /** Evaluates to Undefined if A has a null span, otherwise to False */
  def NullSpan(A : IndexedSymbol) : Constraint  = 
    Less(FirstRow(A), FirstRow(A), 0)

}
