package proofpeer.indent

import scala.language.implicitConversions

object Constraints {
  
  import API.IndexedSymbol
      
  sealed abstract class Constraint[S]
  case class And[S](constraints : List[Constraint[S]]) extends Constraint[S]
  case class Or[S](constraints : List[Constraint[S]]) extends Constraint[S]
  case class Not[S](constraint : Constraint[S]) extends Constraint[S]
  case class Implies[S](assumption : Constraint[S], conclusion : Constraint[S]) 
    extends Constraint[S]
  case class Layout[S](constraint : LayoutConstraint[S]) extends Constraint[S]
  
  def and(constraints : Constraint[IndexedSymbol]*) : Constraint [IndexedSymbol] = 
    And(constraints.toList)
    
  def or(constraints : Constraint[IndexedSymbol]*) : Constraint [IndexedSymbol] = 
    Or(constraints.toList)  
    
  def not(constraint : Constraint[IndexedSymbol]) : Constraint [IndexedSymbol] = 
    Not(constraint)
    
  def implies(asm : Constraint[IndexedSymbol], concl : Constraint[IndexedSymbol]) 
        : Constraint [IndexedSymbol] =
    Implies(asm, concl)      
    
  def ifThenElse(asm : Constraint[IndexedSymbol], thenConcl : Constraint[IndexedSymbol], 
        elseConcl : Constraint[IndexedSymbol]) : Constraint [IndexedSymbol] =
    and(implies(asm, thenConcl), implies(not(asm), elseConcl))          
  
  def Unconstrained[S] : Constraint[S] = And[S](List())
    
  sealed abstract class LayoutConstraint[S]  
  
  /** left < right + delta */
  case class Less[S](left : LayoutEntity[S], right : LayoutEntity[S], delta : Int) 
    extends LayoutConstraint[S]
  
  /** left <= right + delta */
  case class Leq[S](left : LayoutEntity[S], right : LayoutEntity[S], delta : Int) 
    extends LayoutConstraint[S]

  /** left = right + delta */
  case class Eq[S](left : LayoutEntity[S], right : LayoutEntity[S], delta : Int) 
    extends LayoutConstraint[S]
  
  implicit def layoutConstraint2Constraint[S](constraint : LayoutConstraint[S]) : Constraint[S] = {
    Layout(constraint)
  }
  
  /** Denotes integer typed properties of the geometry of s */
  sealed abstract class LayoutEntity[S] {
    def s : S
  }
  
  /** Row of the first line of s. */
  case class FirstRow[S](s : S) extends LayoutEntity[S]
  
  /** Row of the last line of s. */
  case class LastRow[S](s : S) extends LayoutEntity[S]
  
  /** Column of first non-whitespace character in the first line of s. */ 
  case class LeftMostInFirst[S](s : S) extends LayoutEntity[S]
  
  /** Minimum column in which a character of s appears. */
  case class LeftMost[S](s : S) extends LayoutEntity[S]
  
  /** Minimum column in which a character of s appears which is also in the first line of s. */
  case class LeftMostFirst[S](s : S) extends LayoutEntity[S]
  
  /** Minimum column in which a character of s appears which is not in the first line of s.
    * This value is only defined if s consists of at least two rows. 
    */
  case class LeftMostRest[S](s : S) extends LayoutEntity[S]
  
  /** Maximum column in which a character of s appears which is also in the last line of s. */
  case class RightMostLast[S](s : S) extends LayoutEntity[S]
  
  /** B starts in the same line that A ends in. */
  def SameLine(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] =
    and(Eq(LastRow(A), FirstRow(B), 0))
    
  /** A consists of a single line only. */
  def Line(A : IndexedSymbol) : Constraint [IndexedSymbol] = 
    SameLine(A, A)    
    
  /** A and B both fit in the same single line. */
  def Line(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] =
    Eq(FirstRow(A), LastRow(B), 0)
  
  /** B follows A without any whitespace separating A and B. */
  def Connect(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] =
    and(SameLine(A, B), Eq(RightMostLast(A), LeftMostFirst(B), -1))
 
  /** There is no non-whitespace before A in the first line of A,
      and no other line of A is indented more than the first line. 
    */
  def First(A : IndexedSymbol) : Constraint [IndexedSymbol] = 
    and(Eq(LeftMostInFirst(A), LeftMostFirst(A), 0),
        Leq(LeftMostFirst(A), LeftMostRest(A), 0))
    
  /** The first line of A is indented less than all other lines of A. */
  def Protrude(A : IndexedSymbol) : Constraint [IndexedSymbol] = 
    or(Line(A), and(First(A), Less(LeftMostFirst(A), LeftMostRest(A), 0)))
    
  /** A and B are weakly aligned at their left border. */
  def WeakAlign(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] = 
    Eq(LeftMost(A), LeftMost(B), 0)
  
  /** B is weakly indented relative to A. */
  def WeakIndent(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] =
    Less(LeftMost(A), LeftMost(B), 0)
       
  /** A and B are strongly aligned at their left border. */
  def Align(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] = 
    or(NullSpan(A), NullSpan(B), and(First(A), First(B), WeakAlign(A, B)))
    
  /** B is strongly indented relative to A. */
  def Indent(A : IndexedSymbol, B : IndexedSymbol) : Constraint [IndexedSymbol] =
    or(Eq(LastRow(A), LastRow(B), 0),
       and(First(A), WeakIndent(A, B), Eq(LeftMost(A), LeftMostFirst(A), 0)))
  
  /** Evaluates to Undefined if A has a null span, otherwise to False */
  def NullSpan(A : IndexedSymbol) : Constraint [IndexedSymbol] = 
    Less(FirstRow(A), FirstRow(A), 0)
  
  private def findSymbol[S](s : S, symbols : Vector[S], found : Map[S, Set[Int]]) : 
    (Int, Map[S, Set[Int]]) = 
  {
    var indices : Set[Int] = Set()
    for (index <- 0 to symbols.size - 1) {
      if (symbols(index) == s) indices += index
    }
    val index = if (indices.isEmpty) -1 else indices.head
    found.get(s) match {
      case None => (index, found + (s -> indices))
      case Some(_) => (index, found)
    }
  }
  
  private def translateEntity[S](entity : LayoutEntity[S], index : Int) : LayoutEntity[Int] = {
    entity match {
      case FirstRow(_) => FirstRow(index)
      case LastRow(_) => LastRow(index)
      case LeftMostInFirst(_) => LeftMostInFirst(index)
      case LeftMostFirst(_) => LeftMostFirst(index)
      case LeftMostRest(_) => LeftMostRest(index)
      case RightMostLast(_) => RightMostLast(index)
      case LeftMost(_) => LeftMost(index)
    }
  }
  
  private def translateConstraint[S](symbols : Vector[S], 
      constraint : LayoutConstraint[S], found : Map[S, Set[Int]]) : 
      (LayoutConstraint[Int], Map[S, Set[Int]]) = 
  {
    def translate(left : LayoutEntity[S], right : LayoutEntity[S], 
      f : (LayoutEntity[Int], LayoutEntity[Int]) => LayoutConstraint[Int]) :
      (LayoutConstraint[Int], Map[S, Set[Int]]) =
    {
      val (leftindex, found2) = findSymbol(left.s, symbols, found)
      val (rightindex, found3) = findSymbol(right.s, symbols, found2)
      val c = f (translateEntity(left, leftindex), translateEntity(right, rightindex))
      (c, found3)
    }   
    constraint match {
      case Less(left, right, delta) => translate(left, right, (l, r) => Less(l, r, delta))
      case Leq(left, right, delta) => translate(left, right, (l, r) => Leq(l,r, delta))
      case Eq(left, right, delta) => translate(left, right, (l, r) => Eq(l, r, delta))
    }
  }
  
  def translateConstraints[S](symbols : Vector[S],
      constraints : List[Constraint[S]], found : Map[S, Set[Int]]) : 
      (List[Constraint[Int]], Map[S, Set[Int]]) = 
  {
    var f = found
    var result = List[Constraint[Int]]()
    for (c <- constraints) {
      val (tc, tf) = translateConstraint(symbols, c, f)
      f = tf
      result = tc :: result
    }
    (result.reverse, f)
  }
   
  def translateConstraint[S](symbols : Vector[S],
      constraint : Constraint[S], found : Map[S, Set[Int]]) : 
      (Constraint[Int], Map[S, Set[Int]]) = 
  {
    constraint match {
      case And(constraints) => 
        val (cs, f) = translateConstraints(symbols, constraints, found)
        (And(cs), f)
      case Or(constraints) => 
        val (cs, f) = translateConstraints(symbols, constraints, found)
        (Or(cs), f)
      case Not(constraint) =>
        val (c, f) = translateConstraint(symbols, constraint, found)
        (Not(c), f)
      case Implies(assumption, conclusion) =>
        val (List(a, c), f) = translateConstraints(symbols, List(assumption, conclusion), found)
        (Implies(a, c), f)
      case Layout(constraint) => 
        val (c, f) = translateConstraint(symbols, constraint, found)
        (Layout(c), f)
    }
  }
     
}