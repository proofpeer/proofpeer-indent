package proofpeer.indent.earley

import proofpeer.indent._

final case class CoreItem(val nonterminal : Int, val ruleindex : Int, val dot : Int) {
  var nextSymbol : Int = 0 
  var nextSymbolIsNullable : Boolean = false
  var nextCoreItem : Int = -1
  var predictedCoreItems : Array[Int] = null
} 

final class EarleyAutomaton(grammar : Grammar) {

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

  val coreItems : Array[CoreItem] = {
    var states : Map[Int, CoreItem] = Map()
    var idOfCoreItem : Map[CoreItem, Int] = Map()
    for ((symbol, rules) <- grammar.parserules) {
      var ruleindex = 0
      val nonterminal = idOfNonterminal(symbol)
      for (rule <- rules) {
        for (dot <- 0 to rule.rhs.size) {
          val id = states.size
          val coreItem = new CoreItem(nonterminal, ruleindex, dot)
          coreItem.nextSymbol = 
            if (dot == rule.rhs.size) 0 else idOfSymbol(rule.rhs(dot).symbol)
          coreItem.nextSymbolIsNullable  = 
            if (coreItem.nextSymbol <= 0) false else grammar.nullableNonterminals.contains(rule.rhs(dot).symbol)
          coreItem.nextCoreItem = 
            if (coreItem.nextSymbol == 0) -1 else id + 1
          states += (id -> coreItem)
          idOfCoreItem += (coreItem -> id)
        }
        ruleindex += 1
      }
    }
    var coreItems = new Array[CoreItem](states.size)
    for ((id, item) <- states) {
      coreItems(id) = item
      if (item.nextSymbol > 0) {
        val n = nonterminalOfId(item.nextSymbol)
        val rules = grammar.parserules(n)
        item.predictedCoreItems = new Array(rules.size)
        var ruleindex = 0
        for (rule <- rules) {
          val coreItem = new CoreItem(item.nextSymbol, ruleindex, 0)
          item.predictedCoreItems(ruleindex) = idOfCoreItem(coreItem)
          ruleindex += 1
        }
      } else {
        item.predictedCoreItems = new Array(0)
      }
    }
    coreItems
  }

}