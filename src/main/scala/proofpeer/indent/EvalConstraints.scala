package proofpeer.indent

import Constraints._

object EvalConstraints {
  
  type Spans[S] = S => Option[Span]
  
  sealed abstract class Evaluation
  case object Undefined extends Evaluation
  case object True extends Evaluation
  case object False extends Evaluation
    
  def eval[S](constraint : Constraint[S], spans : Spans[S]) : Evaluation = {
    constraint match {
      case And(constraints) => 
        constraints.map(eval(_, spans)).fold(True : Evaluation) {
          case (False, _) => False
          case (_, False) => False
          case (True, True) => True
          case _ => Undefined
        }
      case Or(constraints) => 
        constraints.map(eval(_, spans)).fold(False : Evaluation) {
          case (True, _) => True
          case (_, True) => True
          case (False, False) => False
          case _ => Undefined
        }
      case Not(constraint) =>
        eval(constraint, spans) match {
          case True => False
          case False => True
          case Undefined => Undefined
        }
      case Implies(assumption, conclusion) =>
        (eval(assumption, spans), eval(conclusion, spans)) match {
          case (False, _) => True
          case (_, True) => True
          case (True, False) => False
          case _ => Undefined
        }
      case Layout(constraint) => eval(constraint, spans)
    }
  }
  
  def eval[S](constraint : LayoutConstraint[S], spans : Spans[S]) : Evaluation = {
    constraint match {
      case Less(u, v, delta) =>
        (eval(u, spans), eval(v, spans)) match {
          case (Some(u), Some(v)) => if (u < v + delta) True else False
          case _ => Undefined
        }
      case Leq(u, v, delta) =>
        (eval(u, spans), eval(v, spans)) match {
          case (Some(u), Some(v)) => if (u <= v + delta) True else False
          case _ => Undefined
        }
      case Eq(u, v, delta) =>
        (eval(u, spans), eval(v, spans)) match {
          case (Some(u), Some(v)) => if (u == v + delta) True else False
          case _ => Undefined
        }
    }
  }
  
  def eval[S](entity : LayoutEntity[S], spans : Spans[S]) : Option[Int] = {
    spans(entity.s) match {
      case None => None
      case Some(span) =>
        entity match {
          case FirstRow(_) => Some(span.firstRow)
          case LastRow(_) => Some(span.lastRow)
          case LeftMostInFirst(_) => Some(span.leftMostInFirst)
          case LeftMost(_) => Some(span.leftMost)
          case LeftMostFirst(_) => Some(span.leftMostFirst)
          case LeftMostRest(_) => span.leftMostRest
          case RightMostLast(_) => Some(span.rightMostLast)
      }
    }
  }
  
}