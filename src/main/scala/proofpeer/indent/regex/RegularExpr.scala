package proofpeer.indent.regex

import proofpeer.indent.Range

sealed trait RegularExpr

object RegularExpr {

  case object EMPTY extends RegularExpr

  case class CHAR(chars : Set[Int], positive : Boolean) extends RegularExpr

  case class ALT(left : RegularExpr, right : RegularExpr) extends RegularExpr
  
  case class SEQ(first : RegularExpr, second : RegularExpr) extends RegularExpr
  
  case class OPT(expr : RegularExpr) extends RegularExpr
  
  case class REPEAT(expr : RegularExpr) extends RegularExpr
  
  case class REPEAT1(expr : RegularExpr) extends RegularExpr

  val SPACE = CHAR(Set(32), true)
  
  val NEWLINE = CHAR(Set(10), true)
  
  val WHITESPACE = REPEAT1(ALT(SPACE, NEWLINE))

  case class Rule(priority : Option[Int], expr : RegularExpr)

  type Grammar = Map[Int, RegularExpr]

}

import RegularExpr._

object Tools {

  def merge[K, V](m1 : Map[K, V], m2 : Map[K, V], join : (V, V) => V) : Map[K, V] = {
    var m = m1
    for ((k, v) <- m2) {
      m1.get(k) match {
        case None => 
          m = m + (k -> v)
        case Some(u) =>
          m = m + (k -> join(u, v))
      }
    }
    m
  }

}

object NFA {

  type TokenId = Int
  type State = Int
  type FinalStates = Map[State, Set[TokenId]]
  type InputSymbols = Option[CHAR] 
  type Transitions = Map[State, Map[InputSymbols, Set[State]]]

  def joinTransitions(trans1 : Transitions, trans2 : Transitions) : Transitions = 
    Tools.merge(trans1, trans2, 
      (t1 : Map[InputSymbols, Set[State]], t2 : Map[InputSymbols, Set[State]]) 
        => Tools.merge(t1, t2, (s1 : Set[State], s2 : Set[State]) => s1 ++ s2))

  // all states in the created automaton must be >= startState
  def fromRegularExpr(tokenId : TokenId, expr : RegularExpr, startState : Int) : NFA = {
    expr match {
      case EMPTY =>
        val finalStates : FinalStates = Map(startState -> Set(tokenId))
        val transitions : Transitions = Map(startState -> Map(None -> Set(startState)))
        new NFA(startState, startState, finalStates, transitions)
      case c : CHAR =>
        val endState = startState + 1
        val finalStates : FinalStates = Map(endState -> Set(tokenId))
        val transitions : Transitions = Map(startState -> Map(Some(c) -> Set(endState)))
        new NFA(startState, startState, finalStates, transitions)
      case ALT(left, right) =>
        val leftNFA = fromRegularExpr(tokenId, left, startState + 1)
        val rightNFA = fromRegularExpr(tokenId, right, leftNFA.maxState + 1)
        val transitions : Transitions = Map(startState ->
          Map(None -> Set(leftNFA.startState, rightNFA.startState)))
        new NFA(startState, rightNFA.maxState, 
          leftNFA.finalStates ++ rightNFA.finalStates,
          transitions ++ leftNFA.transitions ++ rightNFA.transitions)
      case SEQ(first, second) =>
        val firstNFA = fromRegularExpr(tokenId, first, startState)
        val secondNFA = fromRegularExpr(tokenId, second, firstNFA.maxState + 1)
        val transitions : Transitions = firstNFA.finalStates.mapValues(c => 
          Map(None -> Set(secondNFA.startState)))
        new NFA(firstNFA.startState, secondNFA.maxState,
          secondNFA.finalStates,
          joinTransitions(firstNFA.transitions, transitions) ++ secondNFA.transitions)
      case REPEAT(expr) =>
        val nfa = fromRegularExpr(tokenId, expr, startState + 1)
        val transitions : Transitions = nfa.finalStates.mapValues(c => 
          Map(None -> Set(startState)))
        new NFA(startState, nfa.maxState, Map(startState -> Set(tokenId)),
          joinTransitions(nfa.transitions, transitions))
      case REPEAT1(expr) => fromRegularExpr(tokenId, SEQ(expr, REPEAT(expr)), startState)
      case OPT(expr) => fromRegularExpr(tokenId, ALT(EMPTY, expr), startState)
    }
  }

  /*def overlap(i1 : InputSymbols, i2 : InputSymbols) : Boolean = {
    (i1, i2) match {
      case (None, None) => true
      case (Some(CHAR(yes1, no1)), Some(CHAR(yes2, no2))) =>

      case _ => false
    }
  } */

}

case class NFA(startState : NFA.State, maxState : NFA.State, finalStates : NFA.FinalStates, 
  val transitions : NFA.Transitions) 
{
  import NFA._

/*  def move(state : State, input : InputSymbols) : Set[State] = {
    transitions.get(state) match {
      case None => Set()
      case Some(transitions) =>
        var dest : Set[State] = Set()
        for ((i, states) <- transitions) {
          if 
        }

    }

  }
  def epsClosure(states : Set[State]) : Set[State] = {
    var closure = states
    var size = 0
    do {
      size = closure.size
      for (state <- states) {
        transitions.get(state) match {
          case None => 
          case Some(transitions) =>

        }
      }
    } while (size != closure.size)
    closure
  }*/
}

object DFA {




}






