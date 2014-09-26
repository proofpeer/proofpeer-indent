package proofpeer.indent

package object layout {

  sealed abstract class Constraint
  final case class And(constraints : List[Constraint]) extends Constraint
  final case class Or(constraints : List[Constraint]) extends Constraint
  final case class Not(constraint : Constraint) extends Constraint
  final case class Implies(assumption : Constraint, conclusion : Constraint) 
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
  final case class Less(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** left <= right + delta */
  final case class Leq(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** left = right + delta */
  final case class Eq(left : LayoutEntity, right : LayoutEntity, delta : Int) 
    extends Constraint

  /** Denotes integer typed properties of the geometry of s */
  sealed abstract class LayoutQualifier {
    def apply(indexedSymbol : IndexedSymbol) : LayoutEntity = (this, indexedSymbol)
    def get(span : Span) : Int 
  }

  /** Row of the first line of s. */
  final case object FirstRow extends LayoutQualifier { def get(s : Span) = s.firstRow }

  /** Row of the last line of s. */
  final case object LastRow extends LayoutQualifier { def get(s : Span) = s.lastRow }

  /** Column of first non-whitespace character in the first line of s. */ 
  final case object LeftMostInFirst extends LayoutQualifier { def get(s : Span) = s.leftMostInFirst }

  /** Minimum column in which a character of s appears. */
  final case object LeftMost extends LayoutQualifier { def get(s : Span) = s.leftMost }

  /** Minimum column in which a character of s appears which is also in the first line of s. */
  final case object LeftMostFirst extends LayoutQualifier { def get(s : Span) = s.leftMostFirst }

  /** Minimum column in which a character of s appears which is not in the first line of s.
    * This value is only defined if s consists of at least two rows. 
    */
  final case object LeftMostRest extends LayoutQualifier { def get(s : Span) = s.leftMostRest }

  /** Maximum column in which a character of s appears which is also in the last line of s. */
  final case object RightMostLast extends LayoutQualifier { def get(s : Span) = s.rightMostLast }

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
