package proofpeer.indent.regex

/** Conversion from regular expressions to NFAs, and from NFAs to DFAs. */

import RegularExpr._
import proofpeer.general.MapUtils

object NFA {

  type TokenId = Int
  type State = Int
  type FinalStates = Map[State, Set[TokenId]]
  type InputSymbols = Option[CHAR] 
  type Transitions = Map[State, Map[InputSymbols, Set[State]]]

  def joinTransitions(trans1 : Transitions, trans2 : Transitions) : Transitions = 
    MapUtils.merge(trans1, trans2, 
      (t1 : Map[InputSymbols, Set[State]], t2 : Map[InputSymbols, Set[State]]) 
        => MapUtils.merge(t1, t2, (s1 : Set[State], s2 : Set[State]) => s1 ++ s2))

  // all states in the created automaton must be >= startState
  def fromRegularExpr(tokenId : TokenId, expr : RegularExpr, startState : Int) : NFA = {
    expr match {
      case NOTHING =>
        new NFA(startState, startState, Map(), Map())
      case EMPTY =>
        val finalStates : FinalStates = Map(startState -> Set(tokenId))
        val transitions : Transitions = Map(startState -> Map(None -> Set(startState)))
        new NFA(startState, startState, finalStates, transitions)
      case c : CHAR =>
        val endState = startState + 1
        val finalStates : FinalStates = Map(endState -> Set(tokenId))
        val transitions : Transitions = Map(startState -> Map(Some(c) -> Set(endState)))
        new NFA(startState, endState, finalStates, transitions)
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
        var transitions : Transitions = nfa.finalStates.mapValues(c => 
          Map(None -> Set(startState))) 
        transitions = transitions + (startState -> Map(None -> Set(nfa.startState)))
        new NFA(startState, nfa.maxState, Map(startState -> Set(tokenId)),
          joinTransitions(nfa.transitions, transitions))
      case REPEAT1(expr) => fromRegularExpr(tokenId, SEQ(expr, REPEAT(expr)), startState)
      case OPT(expr) => fromRegularExpr(tokenId, ALT(EMPTY, expr), startState)
    }
  }

}

case class NFA(startState : NFA.State, maxState : NFA.State, finalStates : NFA.FinalStates, 
  val transitions : NFA.Transitions) 
{
  import NFA._

  def epsMove(state : State) : Set[State] = {
    transitions.get(state) match {
      case None => Set()
      case Some(transitions) =>
        transitions.get(None) match {
          case None => Set()
          case Some(u) => u
        }
    }
  }

  def epsClosure(states : Set[State]) : Set[State] = {
    var closure = states
    var size = 0
    do {
      size = closure.size
      for (state <- closure) closure ++= epsMove(state)
    } while (size != closure.size)
    closure
  }

  def display() {
    println("NFA")
    println("  number of states: " + (maxState - startState + 1))
    println("  number of states with transitions: " + transitions.size)
    println("  number of final states: " + finalStates.size)
    println("  ----")
    for (state <- startState to maxState) {
      val f = finalStates.get(state) match {
        case None => ""
        case Some(ids) => " (final state: " + ids+")"
      }
      println("  state " + state + f + ": ")
      transitions.get(state) match {
        case None => println("    no transitions")
        case Some(transitions) =>
          println("    " + transitions.size + " transitions")
          for ((r, s) <- transitions) {
            println("    transition on " + r + " to " + s)
          } 
      }
    }
  }  

}

class NormalizedTransitions(nfa : NFA) {

  import NFA._

  /** For two pairs (r1, s1) and (r2, s2) in an NT, we have 
    *  - r1 and r2 are disjunct
    *  - s1 and s2 are not equal
    */
  type NT = List[(Range, Set[State])]

  private var normalizedTransitions : Map[State, NT] = Map()

  private def regularTransitions(state : State) : List[(Range, Set[State])] = {
    nfa.transitions.get(state) match {
      case None => List()
      case Some(transitions) =>
        var t : List[(Range, Set[State])] = List()
        for ((symbols, states) <- transitions) {
          symbols match {
            case Some(CHAR(range)) if !states.isEmpty && !range.isEmpty =>
              t :+= (range, nfa.epsClosure(states))
            case _ =>
          }
        }
        t
    }
  }

  def get(state : State) : NT = {
    normalizedTransitions.get(state) match {
      case Some(t) => t
      case None =>
        val t = normalize(regularTransitions(state))
        normalizedTransitions += (state -> t)
        t
    }
  }

  def get(states : Set[State]) : NT = {
    if (states.isEmpty) List()
    else {
      var nt : NT = get(states.head)
      for (state <- states.tail) {
        nt = add(nt, get(state))
      }
      nt
    }
  }

  private def normalize(transitions : List[(Range, Set[State])]) : NT = {
    var nt : NT = List()
    for ((range, states) <- transitions) nt = add(nt, range, states)
    nt
  }

  def add(nt1 : NT, nt2 : NT) : NT = {
    if (nt1.size < nt2.size) add(nt2, nt1)
    else {
      var nt = nt1
      for ((range, states) <- nt2) nt = add(nt, range, states)      
      nt
    }
  }

  def add(nt : NT, range : Range, states : Set[State]) : NT = {
    var m : Map[Set[State], Range] = Map()
    def register(range : Range, states : Set[State]) {
      if (!range.isEmpty && !states.isEmpty) {
        m.get(states) match {
          case None =>
            m += (states -> range)
          case Some(r) =>
            m += (states -> (r + range))
        }
      }
    }
    var rangeSum = Range()
    val crange = -range
    for ((r, ss) <- nt) {
      register(r * range, ss ++ states)
      register(r * crange, ss)
      rangeSum += r
    }
    register(range - rangeSum, states)
    m.toList.map(_.swap)
  }

}

/** startState >= 0 */
case class DFA(startState : DFA.State, maxState : DFA.State, finalStates : DFA.FinalStates,
  transitions : DFA.Transitions)
{
  import DFA._

  /** @return the new state, or -1 if no such state exists */
  def move(state : State, character : Int) : State = {
    transitions.get(state) match {
      case None => -1
      case Some(transitions) =>
        for ((r, s) <- transitions) 
          if (r.contains(character)) return s
        return -1
    }
  }

  def display() {
    println("DFA")
    println("  number of states: " + (maxState - startState + 1))
    println("  number of states with transitions: " + transitions.size)
    println("  number of final states: " + finalStates.size)
    println("  ----")
    for (state <- startState to maxState) {
      val f = finalStates.get(state) match {
        case None => ""
        case Some(ids) => " (final state: " + ids+")"
      }
      println("  state " + state + f + ": ")
      transitions.get(state) match {
        case None => println("    no transitions")
        case Some(transitions) =>
          println("    " + transitions.size + " transitions")
          for ((r, s) <- transitions) {
            println("    transition on " + r + " to " + s)
          } 
      }
    }
  }

}

object DFA {

  type State = NFA.State
  type FinalStates = NFA.FinalStates
  type Transitions = Map[State, List[(Range, State)]]
  type TokenId = NFA.TokenId

  def fromNFA(nfa : NFA) : DFA = {
    val normalizedTransitions = new NormalizedTransitions(nfa)
    val start = nfa.epsClosure(Set(nfa.startState))
    var nodes : Map[Set[NFA.State], Int] = Map(start -> 0)
    var transitions : Map[Set[NFA.State], normalizedTransitions.NT] = Map()
    var newNodes : Set[Set[NFA.State]] = Set(start)
    while (!newNodes.isEmpty) {
      val node = newNodes.head
      newNodes = newNodes - node
      val nt = normalizedTransitions.get(node)
      transitions += (node -> nt)
      for ((_, n) <- nt) {
        if (!nodes.get(n).isDefined) {
          nodes += (n -> nodes.size)
          newNodes += n
        }
      }
    }
    var dfaTransitions : Transitions = Map()
    for ((states, nt) <- transitions) 
      dfaTransitions += (nodes(states) -> nt.map({case (r, states) => (r, nodes(states))}))
    var dfaFinalstates : FinalStates = Map()
    for ((states, node) <- nodes) {
      var tokenIds : Set[TokenId] = Set()
      for (s <- states) {
        nfa.finalStates.get(s) match {
          case Some(ids) => tokenIds ++= ids
          case _ =>
        }
      }
      if (!tokenIds.isEmpty) dfaFinalstates += (node -> tokenIds)
    }
    DFA(nodes(start), nodes.size - 1, dfaFinalstates, dfaTransitions)
  }

  import proofpeer.indent._

  /** @return (l, ids) where l is the length of the recognized token and ids is the set of 
    * ids of recognized tokens. 
    */
  def run(dfa : DFA, characterStream : CharacterStream) : (Int, Set[TokenId]) = {
    var state = dfa.startState
    var recognizedLength = -1
    var recognizedTokens : Set[TokenId] = null
    var len = 0
    while (state >= 0) {
      dfa.finalStates.get(state) match {
        case Some(ids) => 
          recognizedLength = len
          recognizedTokens = ids
        case _ =>
      }
      val character = characterStream.nextCharacter
      len += 1
      if (character < 0) return (recognizedLength, recognizedTokens)
      state = dfa.move(state, character)
    }
    (recognizedLength, recognizedTokens)
  }

}




