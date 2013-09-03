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
  
  def Unconstrained[S] = And[S](List())
    
  sealed abstract class LayoutConstraint[S]  
  case class Less[S](left : LayoutEntity[S], right : LayoutEntity[S]) 
    extends LayoutConstraint[S]
  
  case class Leq[S](left : LayoutEntity[S], right : LayoutEntity[S]) 
    extends LayoutConstraint[S]

  case class Eq[S](left : LayoutEntity[S], right : LayoutEntity[S]) 
    extends LayoutConstraint[S]
  
  implicit def layoutConstraint2Constraint[S](constraint : LayoutConstraint[S]) : Constraint[S] = {
    Layout(constraint)
  }
  
  sealed abstract class LayoutEntity[S] {
    def s : S
  }
  case class FirstRow[S](s : S) extends LayoutEntity[S]
  case class LastRow[S](s : S) extends LayoutEntity[S]
  case class LeftMostInFirst[S](s : S) extends LayoutEntity[S]
  case class LeftMost[S](s : S) extends LayoutEntity[S]
  case class LeftMostFirst[S](s : S) extends LayoutEntity[S]
  case class LeftMostRest[S](s : S) extends LayoutEntity[S]
  case class RightMostLast[S](s : S) extends LayoutEntity[S]
  
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
      case Less(left, right) => translate(left, right, (l, r) => Less(l, r))
      case Leq(left, right) => translate(left, right, (l, r) => Leq(l,r))
      case Eq(left, right) => translate(left, right, (l, r) => Eq(l, r))
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
  
  /** B follows A without any whitespace separating A and B */
  //case class Connect[T](A : T, B : IndexedSymbol) extends LayoutConstraint
  
  /** B starts in the same line that A ends in */
  //case class Line(A : IndexedSymbol, B : IndexedSymbol) extends LayoutConstraint
  
  /** B is indented relative to A */
  //case class Indent(A : IndexedSymbol, B : IndexedSymbol) extends LayoutConstraint  
  
  /** A and B align on the left hand side */
  //case class Align(A : IndexedSymbol, B : IndexedSymbol) extends LayoutConstraint
  
  /** The first line of A is indented less than all other lines of A */
  //case class Protrude(A : IndexedSymbol) extends LayoutConstraint  
  
  /** There is no non-whitespace before A in the first line of A */
  //case class First(A : IndexedSymbol) extends LayoutConstraint
  
}