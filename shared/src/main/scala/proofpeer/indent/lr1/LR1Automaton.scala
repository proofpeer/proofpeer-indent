package proofpeer.indent.lr1

import proofpeer.indent._
import proofpeer.indent.regex.Range

final case class CoreItem(val nonterminal : Int, val ruleindex : Int, val dot : Int) {
  var rhs : Vector[Int] = null
  var nextSymbol : Int = 0 
  var nextCoreItem : Int = -1
} 

final case class LR1Item(coreItemIndex : Int, lookahead : Int) // lookahead of 0 is end of input terminal

trait LR1StateMachine {
  def grammar : Grammar
  def coreItemAt(index : Int) : CoreItem
  def startState : Int
  def startNonterminalId : Int
  def shift(state : Int, x : Int) : Option[Int]
  def reduce(state : Int, x : Int) : Option[Int]
  def lookahead(state : Int) : Set[Int]
  def symbolOfId(id : Int) : String
}

final class LR1Automaton(val grammar : Grammar, val startNonterminal : String) extends LR1StateMachine {

  type LR1State = Set[LR1Item]

  // terminal ids are < 0
  val (idOfTerminal, terminalOfId) = {
    var idOfTerminal : Map[String, Int] = Map()
    var terminalOfId : Map[Int, String] = Map()
    var id = 0
    for (terminal <- grammar.terminals) {
      id -= 1
      idOfTerminal += (terminal -> id)
      terminalOfId += (id -> terminal)
    }
    (idOfTerminal, terminalOfId)
  }

  // nonterminal ids are > 0
  val (idOfNonterminal, nonterminalOfId) = {
    var idOfNonterminal : Map[String, Int] = Map()
    var nonterminalOfId : Map[Int, String] = Map()
    var id = 0
    for (nonterminal <- grammar.nonterminals) {
      id += 1
      idOfNonterminal += (nonterminal -> id)
      nonterminalOfId += (id -> nonterminal)
    }
    (idOfNonterminal, nonterminalOfId)
  }

  def idOfSymbol(symbol : String) : Int = {
    idOfTerminal.get(symbol) match {
      case None => idOfNonterminal(symbol)
      case Some(id) => id
    }
  }

  val (coreItems : Array[CoreItem], idOfCoreItem : Map[CoreItem, Int], startCoreItemsOf : Map[Int, Set[Int]]) = {
    var states : Map[Int, CoreItem] = Map()
    var idOfCoreItem : Map[CoreItem, Int] = Map()
    var startCoreItemsOf : Map[Int, Set[Int]] = Map()
    for ((symbol, rules) <- grammar.parserules) {
      var ruleindex = 0
      val nonterminal = idOfNonterminal(symbol)
      var startCoreItems : Set[Int] = Set()
      for (rule <- rules) {
        val rhs = rule.rhs.map(x => idOfSymbol(x.symbol))
        val rhsSymbols = rule.rhs.map(x => x.symbol)
        val rhsIndices = grammar.rhsIndices(symbol, ruleindex)
        for (dot <- 0 to rule.rhs.size) {
          val id = states.size
          val coreItem = new CoreItem(nonterminal, ruleindex, dot)
          coreItem.rhs = rhs
          coreItem.nextSymbol = 
            if (dot == rule.rhs.size) 0 else idOfSymbol(rule.rhs(dot).symbol)
          coreItem.nextCoreItem = 
            if (coreItem.nextSymbol == 0) -1 else id + 1
          states += (id -> coreItem)
          idOfCoreItem += (coreItem -> id)
          if (dot == 0) {
            startCoreItems += id
          }
        }
        ruleindex += 1
      }
      startCoreItemsOf += (nonterminal -> startCoreItems)
    }
    var coreItems = new Array[CoreItem](states.size)
    for ((id, item) <- states) {
      coreItems(id) = item
    }
    (coreItems, idOfCoreItem, startCoreItemsOf)
  }

  def symbolOfId(id : Int) : String = {
    if (id < 0) terminalOfId(id) else nonterminalOfId(id)
  }

  def coreItem2String(coreItem : CoreItem) : String = {
    var s = ""
    s = s + symbolOfId(coreItem.nonterminal) + " => "
    var i = 0
    while (i < coreItem.rhs.length) {
      if (i == coreItem.dot) s = s + ". "
      s = s + symbolOfId(coreItem.rhs(i)) + " "
      i = i + 1
    }
    if (i == coreItem.dot) s = s + ". "
    s
  }

  def START(ids : Seq[Int]) : Set[Int] = {
    val X = ids(0)
    if (X <= 0) Set(X)
    else {
      val nonterminal = nonterminalOfId(X)
      val start = grammar.START(Seq(nonterminal)).map(idOfTerminal(_))
      if (grammar.isNullableNonterminal(nonterminal))
        start ++ START(ids.drop(1))
      else
        start
    }
  }

  def closureOfItems(items : LR1State) : LR1State = {
    var state = items
    var oldsize : Int = 0
    var size = state.size
    do {
      oldsize = size
      for (item <- state) {
        val coreItem = coreItems(item.coreItemIndex)
        if (coreItem.nextSymbol > 0) {
          val s = coreItem.rhs.drop(coreItem.dot + 1) :+ item.lookahead
          val coreItemIndices = startCoreItemsOf(coreItem.nextSymbol)
          for (lookahead <- START(s)) {
            for (coreItemIndex <- coreItemIndices) {
              state += LR1Item(coreItemIndex, lookahead)
            }
          }
        }
      }
      size = state.size
    } while (size != oldsize)
    state
  }

  def initialState(nonterminalId : Int) : LR1State = {
    var items : LR1State = Set()
    for (index <- startCoreItemsOf(nonterminalId)) {
      items += LR1Item(index, 0)
    }
    closureOfItems(items)
  }

  def succStates(state : LR1State) : Map[Int, LR1State] = {
    var succs : Map[Int, LR1State] = Map()
    for (item <- state) {
      val coreItem = coreItems(item.coreItemIndex)
      val X = coreItem.nextSymbol
      if (X != 0) {
        val before : LR1State = succs.get(X) match { case Some(items) => items case None => Set() }
        succs += (X -> (before + LR1Item(coreItem.nextCoreItem, item.lookahead)))
      }
    }
    succs.mapValues(closureOfItems _)
  }

  private def computeReduce(state : LR1State) : Map[Int, Set[Int]] = {
    var r : Map[Int, Set[Int]] = Map()
    for (item <- state) {
      val coreItem = coreItems(item.coreItemIndex)
      if (coreItem.nextSymbol == 0) {
        r.get(item.lookahead) match {
          case None => r += (item.lookahead -> Set(item.coreItemIndex))
          case Some(indices) => r += (item.lookahead -> (indices + item.coreItemIndex))
        }
      }
    }
    r
  }

  type LR1Graph =  Map[Int, (LR1State, Map[Int, Int], Map[Int, Set[Int]], Set[Int])]

  def computeLR1Graph(nonterminalId : Int) : LR1Graph = {
    var states : Map[LR1State, Int] = Map()
    var queue : Vector[Int] = Vector()
    var graph : Map[Int, (LR1State, Map[Int, Int])] = Map()
    def idOfState(state : LR1State) : Int = {
      states.get(state) match {
        case Some(id) => id
        case None =>
          val id = states.size
          states += (state -> id)
          graph += (id -> (state, Map()))
          queue = queue :+ id
          id
      }
    }
    idOfState(initialState(nonterminalId))
    while (!queue.isEmpty) {
      val id = queue(0)
      queue = queue.drop(1)
      var (state, succs) = graph(id)
      for ((x, succX) <- succStates(state)) {
        val succId = idOfState(succX)
        succs += (x -> succId)
      }
      graph += (id -> (state, succs))
    }
    def node(state : LR1State, succs : Map[Int, Int]) : (LR1State, Map[Int, Int], Map[Int, Set[Int]], Set[Int]) = {
      val reductions = computeReduce(state)
      (state, succs, reductions, succs.keySet.filter(_ < 0) ++ reductions.keySet)
    }
    graph.mapValues(v => node(v._1, v._2))
  }

  val graph : LR1Graph = computeLR1Graph(idOfNonterminal(startNonterminal))

  def isConsistent(g : LR1Graph, stateIndex : Int) : Boolean = {
    var (_, succs, reductions, _) = g(stateIndex)
    for ((x, indices) <- reductions) {
      if (indices.size > 1) return false
      if (indices.size == 0) reductions -= x
    }
    succs.keySet.intersect(reductions.keySet).size == 0
  }

  def isConsistent(g : LR1Graph) : Boolean = {
    for (stateIndex <- graph.keys)
      if (!isConsistent(graph, stateIndex)) return false
    true
  }

  val consistent = isConsistent(graph)

  def coreItemAt(index : Int) : CoreItem = coreItems(index)

  def startState : Int = 0

  def shift(state : Int, x : Int) : Option[Int] = graph(state)._2.get(x)

  def reduce(state : Int, x : Int) : Option[Int] = 
    graph(state)._3.get(x) match {
      case None => None
      case Some(reductions) => 
        if (reductions.size == 1) Some(reductions.head) else None
    }

  def lookahead(state : Int) : Set[Int] = graph(state)._4

  val startNonterminalId : Int = idOfNonterminal(startNonterminal)


}