package proofpeer.indent.earley

import proofpeer.indent.Span

object HyperEarleyAutomaton {

  sealed trait Bin {
    def select(origin : Int, current : Int) : Int
  }
  final case object B extends Bin {
    def select(origin : Int, current : Int) = origin
  }
  final case object CURRENT extends Bin {
    def select(origin : Int, current : Int) = current
  }

  sealed trait Layout {
    def realize(layout : Span.Layout) : Span.Layout
  }
  final case object L extends Layout {
    def realize(layout : Span.Layout) : Span.Layout = layout
  }
  final case object ZERO extends Layout {
    def realize(layout : Span.Layout) : Span.Layout = null    
  }
  final case class PlusEpsilon(layout : Layout) extends Layout {
    def realize(l : Span.Layout) : Span.Layout = {
      Span.addToLayout(layout.realize(l), null)
    }    
  }

  final case class Item(val coreItemId : Int, val origin : Bin, val layout : Layout)

}

final case class HyperCoreItem(val coreItemIds : Set[Int]) {
  import HyperEarleyAutomaton._
  var terminals : Set[Int] = null
  var terminalTransitions : Map[Int, List[(Int, Bin, Layout)]] = null
  var nonterminalTransitions : Map[Int, List[(Int, Bin, Layout)]] = null
  var completedNonterminals : Map[Int, (Boolean, Vector[CoreItem])] = null
} 

final class HyperEarleyAutomaton(val ea : EarleyAutomaton, val startNonterminal : Int) {
  import HyperEarleyAutomaton._

  def closure(_items : Set[Item]) : Set[Item] = {
    var items = _items
    var size = 0
    do {
      size = items.size
      for (item <- items) {
        val coreItem = ea.coreItems(item.coreItemId)
        for (predictedCoreItemId <- coreItem.predictedCoreItems) {
          items += Item(predictedCoreItemId, CURRENT, ZERO)
        }
        if (coreItem.nextSymbolIsNullable) {
          items += Item(coreItem.nextCoreItem, item.origin, PlusEpsilon(item.layout))
        }
      }
    } while (size != items.size)
    items
  }

  def separate(items : Set[Item]) : Map[(Bin, Layout), Set[Int]] = {
    var classes : Map[(Bin, Layout), Set[Int]] = Map()
    for (item <- items) {
      val c = (item.origin, item.layout)
      classes.get(c) match {
        case None => classes += (c -> Set(item.coreItemId))
        case Some(coreItemIds) => classes += (c -> (coreItemIds + item.coreItemId))
      }
    }
    classes
  }

  def startItems : Set[Item] = {
    val n = ea.nonterminalOfId(startNonterminal)
    val items = 
      for (i <- 0 until ea.grammar.parserules(n).size) 
        yield Item(ea.idOfCoreItem(CoreItem(startNonterminal, i, 0)), CURRENT, ZERO)
    items.toSet
  }

  var states : Map[Set[Int], Int] = Map()
  var coreItemIdsOfState : Map[Int, Set[Int]] = Map()
  var startStates : List[(Int, Layout)] = List()
  var transitions : Map[Int, Map[Int, List[(Int, Bin, Layout)]]] = Map()

  def addState(items : Set[Int]) : Int = {
    states.get(items) match {
      case None => 
        val size = states.size
        states += (items -> size)
        coreItemIdsOfState += (size -> items)
        transitions += (size -> Map())
        size
      case Some(i) => i
    }
  }

  val eaSymbolIds = ea.terminalOfId.keys ++ ea.nonterminalOfId.keys

  def processState(state : Int) {
    var ts : Map[Int, List[(Int, Bin, Layout)]] = Map()
    val coreItemIds = coreItemIdsOfState(state)
    for (symbolId <- eaSymbolIds) {
      var targetItems : Set[Item] = Set()
      for (coreItemId <- coreItemIds) {
        val coreItem = ea.coreItems(coreItemId)
        if (coreItem.nextSymbol == symbolId) {
          targetItems += Item(coreItem.nextCoreItem, B, L)
        }
      }  
      if (!targetItems.isEmpty) {
        val targets = separate(closure(targetItems)).toList.map(x => (addState(x._2), x._1._1, x._1._2))
        ts += (symbolId -> targets)
      }    
    }
    transitions += (state -> ts)
  }

  def computeAutomaton() {
    startStates = separate(closure(startItems)).toList.map(x => (addState(x._2), x._1._2))
    var processed = 0
    while (processed < states.size) {
      processState(processed)
      processed += 1
    }
  }

  def printClasses(classes : Map[(Bin, Layout), Set[Int]]) {
    println("number of classes: " + classes.size)
    for (((bin, layout), coreItemIds) <- classes) {
      println("  bin: " + bin, "layout: " + layout + ", size: " + coreItemIds.size)
    }
  }

  val (hyperCoreItems : Array[HyperCoreItem]) = {
    computeAutomaton()
    val hyperCoreItems : Array[HyperCoreItem] = new Array(states.size)
    for ((coreItemIds, stateId) <- states) {
      val item = new HyperCoreItem(coreItemIds)
      item.terminals = Set()
      item.terminalTransitions = Map()
      item.nonterminalTransitions = Map()
      for ((symbolId, targets) <- transitions(stateId)) {
        if (symbolId < 0) {
          item.terminals += symbolId
          item.terminalTransitions += (symbolId -> targets)
        } else {
          item.nonterminalTransitions += (symbolId -> targets)
        } 
      }
      item.completedNonterminals = Map()
      for (coreItemId <- coreItemIds) {
        val coreItem = ea.coreItems(coreItemId)
        if (coreItem.nextSymbol == 0) {
          item.completedNonterminals.get(coreItem.nonterminal) match {
            case None =>
              item.completedNonterminals += (coreItem.nonterminal -> (coreItem.unconstrained, Vector(coreItem)))
            case Some((unconstrained, coreItems)) =>
              item.completedNonterminals += (coreItem.nonterminal -> (coreItem.unconstrained && unconstrained, coreItems :+ coreItem))
          }
        }
      }
      hyperCoreItems(stateId) = item
    }
    hyperCoreItems
  }
  
}