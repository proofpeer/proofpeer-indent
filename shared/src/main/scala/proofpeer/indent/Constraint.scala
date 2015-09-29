package proofpeer.indent

sealed trait Constraint 

object Constraint {

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
    
  sealed trait LayoutValue 
  case class LayoutEntity(q : LayoutQualifier, s : IndexedSymbol) extends LayoutValue
  case object Parameter extends LayoutValue
  case object Zero extends LayoutValue

  /** left < right + delta */
  final case class Less(left : LayoutValue, right : LayoutValue, delta : Int) 
    extends Constraint

  /** left <= right + delta */
  final case class Leq(left : LayoutValue, right : LayoutValue, delta : Int) 
    extends Constraint

  /** left = right + delta */
  final case class Eq(left : LayoutValue, right : LayoutValue, delta : Int) 
    extends Constraint

  final case class NullSpan(symbol : IndexedSymbol) extends Constraint

  /** Denotes integer typed properties of the geometry of s */
  sealed abstract class LayoutQualifier {
    def apply(indexedSymbol : IndexedSymbol) : LayoutEntity = LayoutEntity(this, indexedSymbol)
    def get(span : Span) : Option[Int] 
  }

  /** Row of the first line of s. */
  final case object FirstRow extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.firstRow) }

  /** Row of the last line of s. */
  final case object LastRow extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.lastRow) }

  /** Maximum number of consecutive empty rows within s. */
  final case object MaxRowGap extends LayoutQualifier { def get(s : Span) = Some(s.maxRowGap) }

  /** Column of first non-whitespace character in the first line of s. */ 
  final case object LeftMostInFirst extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.leftMostInFirst) }

  /** Minimum column in which a character of s appears. */
  final case object LeftMost extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.leftMost) }

  /** Minimum column in which a character of s appears which is also in the first line of s. */
  final case object LeftMostFirst extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.leftMostFirst) }

  /** Minimum column in which a character of s appears which is not in the first line of s.
    * This value is only defined if s consists of at least two rows. 
    */
  final case object LeftMostRest extends LayoutQualifier { def get(s : Span) = if (s.isNull || s.firstRow == s.lastRow) None else Some(s.leftMostRest) }

  /** Maximum column in which a character of s appears which is also in the last line of s. */
  final case object RightMostLast extends LayoutQualifier { def get(s : Span) = if (s.isNull) None else Some(s.rightMostLast) }

  /** B starts in the same line that A ends in. */
  def SameLine(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    Eq(LastRow(A), FirstRow(B), 0)
    
  /** A consists of a single line only. */
  def Line(A : IndexedSymbol) : Constraint = 
    SameLine(A, A)    
    
  /** A and B both fit in the same single line. */
  def Line(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    Eq(FirstRow(A), LastRow(B), 0)

  /** B follows A without any whitespace separating A and B. */
  def Connect(A : IndexedSymbol, B : IndexedSymbol) : Constraint  =
    and(SameLine(A, B), Eq(RightMostLast(A), LeftMostFirst(B), -1))

  def connect(A : IndexedSymbol, B : IndexedSymbol*) : Constraint = {
    var constraints : List[Constraint] = List()
    var last : IndexedSymbol = A
    for (b <- B) {
      constraints = Connect(last, b) :: constraints
      last = b
    }
    and(constraints : _*)
  }

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

  def collectSymbols(constraint : Constraint) : Set[IndexedSymbol] = {
    constraint match {
      case And(cs) => collectSymbols(cs)
      case Or(cs) => collectSymbols(cs)
      case Not(c) => collectSymbols(c)
      case Implies(p, q) => collectSymbols(List(p, q))
      case Less(left, right, _) => collectSymbols(left) ++ collectSymbols(right)
      case Leq(left, right, _) => collectSymbols(left) ++ collectSymbols(right)
      case Eq(left, right, _) => collectSymbols(left) ++ collectSymbols(right)
      case NullSpan(symbol) => Set(symbol)
    }
  }

  def collectSymbols(v : LayoutValue) : Set[IndexedSymbol] = {
    v match {
      case LayoutEntity(_, s) => Set(s)
      case _ => Set()
    }
  }

  def collectSymbols(constraints : List[Constraint]) : Set[IndexedSymbol] = {
    var symbols : Set[IndexedSymbol] = Set()
    for (c <- constraints) symbols ++= collectSymbols(c)
    symbols
  }

  type L = Span.Layout
  type Evaluator = (Int, L) => Option[Boolean]
  type Measure = (Int, L) => Option[Int]

  private def AndEval(u : Evaluator, v : Evaluator) : Evaluator = (p : Int, layout : L) => 
    (u(p, layout), v(p, layout)) match {
      case (Some(p), Some(q)) => Some(p && q)
      case (_, Some(false)) => Some(false)
      case (Some(false), _) => Some(false)
      case _ => None
    }

  private def PartialAndEval(u : Evaluator) : Evaluator = (p : Int, layout : L) =>
    u(p, layout) match {
      case Some(false) => Some(false)
      case _ => None
    }

  private def OrEval(u : Evaluator, v : Evaluator) : Evaluator = (p : Int, layout : L) => 
    (u(p, layout), v(p, layout)) match {
      case (Some(p), Some(q)) => Some(p || q)
      case (_, Some(true)) => Some(true)
      case (Some(true), _) => Some(true)
      case _ => None
    }

  private def PartialOrEval(u : Evaluator) : Evaluator = (p : Int, layout : L) =>
    u(p, layout) match {
      case Some(true) => Some(true)
      case _ => None
    }

  private def NotEval(u : Evaluator) : Evaluator = (p : Int, layout : L) => 
    u(p, layout) match {
      case None => None
      case Some(p) => Some(!p)
    }

  private def ImpliesEval(u : Evaluator, v : Evaluator) : Evaluator = (p : Int, layout : L) => 
    (u(p, layout), v(p, layout)) match {
      case (Some(p), Some(q)) => Some(if (p) q else true)
      case (Some(false), _) => Some(true)
      case (_, Some(true)) => Some(true)
      case _ => None
    }

  private def PartialConclEval(u : Evaluator) : Evaluator = (p : Int, layout : L) =>
    u(p, layout) match {
      case Some(true) => Some(true)
      case _ => None
    }

  private def PartialHypEval(u : Evaluator) : Evaluator = (p : Int, layout : L) =>
    u(p, layout) match {
      case Some(false) => Some(true)
      case _ => None
    }

  private def LessEval(left : Measure, right : Measure, delta : Int) : Evaluator = 
    (p : Int, layout : L) =>
      (left(p, layout), right(p, layout)) match {
        case (Some(left), Some(right)) => Some(left < right + delta)
        case _ => None
      }

  private def LeqEval(left : Measure, right : Measure, delta : Int) : Evaluator = 
    (p : Int, layout : L) =>
      (left(p, layout), right(p, layout)) match {
        case (Some(left), Some(right)) => Some(left <= right + delta)
        case _ => None
      }

  private def EqEval(left : Measure, right : Measure, delta : Int) : Evaluator = 
    (p : Int, layout : L) =>
      (left(p, layout), right(p, layout)) match {
        case (Some(left), Some(right)) => Some(left == right + delta)
        case _ => None
      }


  def evalConstraint(constraint : Constraint, f : IndexedSymbol => Option[Int]) : Option[Evaluator] = {
    constraint match {
      case And(List()) => Some((p : Int, layout : L) => Some(true))
      case And(cs) =>
        val constraints = cs.map(c => evalConstraint(c, f))
        var e : Option[Evaluator] = constraints.head
        for (eval <- constraints.tail) {
          e = (e, eval) match {
            case (None, None) => None
            case (None, Some(v)) => Some(PartialAndEval(v))
            case (Some(u), None) => Some(PartialAndEval(u))
            case (Some(u), Some(v)) => Some(AndEval(u, v))
          }
        }
        e
      case Or(List()) => Some((p : Int, layout : L) => Some(false))        
      case Or(cs) =>
        val constraints = cs.map(c => evalConstraint(c, f))
        var e : Option[Evaluator] = constraints.head
        for (eval <- constraints.tail) {
          e = (e, eval) match {
            case (None, None) => None
            case (None, Some(v)) => Some(PartialOrEval(v))
            case (Some(u), None) => Some(PartialOrEval(u))
            case (Some(u), Some(v)) => Some(OrEval(u, v))
          }
        }
        e
      case Implies(u, v) =>
        (evalConstraint(u, f), evalConstraint(v, f)) match {
          case (None, None) => None
          case (None, Some(v)) => Some(PartialConclEval(v))
          case (Some(u), None) => Some(PartialHypEval(u))
          case (Some(u), Some(v)) => Some(ImpliesEval(u, v))
        }
      case Not(u) =>
        (evalConstraint(u, f)) match {
          case None => None
          case Some(u) => Some(NotEval(u))
        }
      case Less(left, right, delta) =>
        (evalLayoutValue(left, f), evalLayoutValue(right, f)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(left), Some(right)) => Some(LessEval(left, right, delta))
        }
      case Leq(left, right, delta) =>
        (evalLayoutValue(left, f), evalLayoutValue(right, f)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(left), Some(right)) => Some(LeqEval(left, right, delta))
        }
      case Eq(left, right, delta) =>
        (evalLayoutValue(left, f), evalLayoutValue(right, f)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(left), Some(right)) => Some(EqEval(left, right, delta))
        }
      case NullSpan(symbol) => 
        f(symbol) match {
          case None => None
          case Some(i) => Some((p : Int, layout : L) => Some(layout(i).isNull))
        }
    }
  }

  private def EvalQualifier(q : LayoutQualifier, i : Int) : Measure = 
    (p : Int, layout : L) => {
      q.get(layout(i))
    }

  private def evalLayoutEntity(entity : LayoutEntity, f : IndexedSymbol => Option[Int]) : Option[Measure] = {
    f(entity.s) match {
      case None => None
      case Some(i) => Some(EvalQualifier(entity.q, i))
    }
  } 

  private def evalLayoutValue(value : LayoutValue, f : IndexedSymbol => Option[Int]) : Option[Measure] = {
    value match {
      case e : LayoutEntity => evalLayoutEntity(e, f)
      case Parameter => Some((p : Int, layout : L) => Some(p))
      case Zero => Some((p : Int, layout : L) => Some(0))
    }
  }

}
