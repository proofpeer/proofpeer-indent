package proofpeer.indent.earley

import proofpeer.indent._
import proofpeer.indent.regex._

final case class CoreItem(val nonterminal : Int, val ruleindex : Int, val dot : Int) {
  var rhs : Vector[Int] = null
  var nextSymbol : Int = 0 
  var nextSymbolIsNullable : Boolean = false
  var nextCoreItem : Int = -1
  var predictedCoreItems : Array[Int] = null
  var evalConstraint : Span.Layout => Boolean = layout => true
} 

final class EarleyAutomaton(val grammar : Grammar) {

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

  val (coreItems : Array[CoreItem], idOfCoreItem : Map[CoreItem, Int]) = {
    var states : Map[Int, CoreItem] = Map()
    var idOfCoreItem : Map[CoreItem, Int] = Map()
    for ((symbol, rules) <- grammar.parserules) {
      var ruleindex = 0
      val nonterminal = idOfNonterminal(symbol)
      for (rule <- rules) {
        val rhs = rule.rhs.map(x => idOfSymbol(x.symbol))
        val rhsIndices = grammar.rhsIndices(symbol, ruleindex)
        for (dot <- 0 to rule.rhs.size) {
          val id = states.size
          val coreItem = new CoreItem(nonterminal, ruleindex, dot)
          coreItem.rhs = rhs
          coreItem.nextSymbol = 
            if (dot == rule.rhs.size) 0 else idOfSymbol(rule.rhs(dot).symbol)
          coreItem.nextSymbolIsNullable  = 
            if (coreItem.nextSymbol <= 0) false else grammar.nullableNonterminals.contains(rule.rhs(dot).symbol)
          coreItem.nextCoreItem = 
            if (coreItem.nextSymbol == 0) -1 else id + 1
          def f(s : IndexedSymbol) : Option[Int] = {
            val i = rhsIndices(s)
            if (i < dot) Some(i) else None
          }
          Constraint.evalConstraint(rule.constraint, f) match {
            case Some(eval) => 
              coreItem.evalConstraint = layout => {
                eval(layout) match {
                  case Some(q) => q
                  case None => true
                }
              }
            case _ =>
          }
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
    (coreItems, idOfCoreItem)
  }

  val (numScopes, dfas, scopeOfTerminal) = {
    var scopes : Map[String, (Int, List[(Int, RegularExpr)])] = Map()
    var scopesOfTerminals : Array[Int] = new Array(terminalOfId.size)
    var scope = 0
    for (terminal <- grammar.terminals) {
      val scanrule = grammar.scanrules(terminal)
      val terminalId = idOfTerminal(terminal)
      val entry = (terminalId, scanrule.regex)
      scopes.get(scanrule.scope) match {
        case None => 
          scopes += (scanrule.scope -> (scope, List(entry)))
          scopesOfTerminals((-terminalId) - 1) = scope
          scope += 1
        case Some((scope, rules)) => 
          scopes += (scanrule.scope -> (scope, entry :: rules))
          scopesOfTerminals((-terminalId) - 1) = scope
      }
    }
    def scopeOfTerminal(terminalId : Int) : Int = {
      scopesOfTerminals((-terminalId) - 1)
    }
    val dfas = new Array[DFA](scopes.size)
    for ((_, (scope, rules)) <- scopes) {
      val nfa = NFA.fromRegularExprs(rules)
      val dfa = DFA.fromNFA(nfa)
      dfas(scope) = dfa
    }
    (scopes.size, dfas, scopeOfTerminal _)
  }

  val terminalPriority = {
    var terminalPriority : Map[Int, Option[Int]] = Map()
    for ((id, terminal) <- terminalOfId) {
      terminalPriority += (id -> grammar.scanrules(terminal).priority)
    }
    terminalPriority
  }

  def prioritizeTerminals(terminals : Set[Int]) : Set[Int] = {
    if (terminals == null || terminals.size <= 1) return terminals
    var nonprio : Set[Int] = Set()
    var highestPrio : Int = Int.MinValue
    var terminalsWithHighestPrio : Set[Int] = Set()
    for (terminal <- terminals) {
      terminalPriority(terminal) match {
        case None =>
          nonprio += terminal
        case Some(prio) =>
          if (highestPrio == prio) {
            terminalsWithHighestPrio += terminal
          } else if (highestPrio < prio) {
            terminalsWithHighestPrio = Set(terminal)
            highestPrio = prio
          }
      }
    }
    nonprio ++ terminalsWithHighestPrio
  }

  def coreItemOf(item : Earley.Item) : CoreItem = coreItems(item.coreItemId)

}