package proofpeer.indent.earley

import proofpeer.indent._

final case class CoreItem(val nonterminal : Int, val ruleindex : Int, val dot : Int) {
  var rhs : Vector[Int] = null
  var nextSymbol : Int = 0 
  var nextSymbolIsNullable : Boolean = false
  var nextCoreItem : Int = -1
  var predictedCoreItems : Array[Int] = null
  var evalConstraint : (ParseParam.V, Span.Layout, ParseParam.Results) => Boolean = 
    (param : ParseParam.V, layout : Span.Layout, results : ParseParam.Results) => true 
  var evalParam : (ParseParam.V, Span.Layout, ParseParam.Results, Int) => ParseParam.V = 
    { case (param, layout, results, i) => Earley.DEFAULT_PARAM }
  var evalResult : (ParseParam.V, Span.Layout, ParseParam.Results) => ParseParam.V = 
    { case (param, layout, results) => Earley.DEFAULT_PARAM }
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
        val evalParam = ParseParam.evaluateParams(rule.params, s => rhsIndices(s))
        val evalResult = ParseParam.evaluateParam(rule.result, s => rhsIndices(s))
        for (dot <- 0 to rule.rhs.size) {
          val id = states.size
          if (Earley.debug)
            println("coreItem " + id + ": " + symbol + " => " + rule.rhs + " (dot = " + dot +")")
          val coreItem = new CoreItem(nonterminal, ruleindex, dot)
          coreItem.rhs = rhs
          coreItem.nextSymbol = 
            if (dot == rule.rhs.size) 0 else idOfSymbol(rule.rhs(dot).symbol)
          coreItem.nextSymbolIsNullable  = 
            if (coreItem.nextSymbol <= 0) false else grammar.nullableNonterminals.contains(rule.rhs(dot).symbol)
          coreItem.nextCoreItem = 
            if (coreItem.nextSymbol == 0) -1 else id + 1
          def f(s : IndexedSymbol) : Option[Int] = {
            if (s.index == None && s.symbol == rule.symbol) {
              if (coreItem.nextSymbol == 0) Some(rule.rhs.size) else None
            } else {
              val i = rhsIndices(s)
              if (i < dot) Some(i) else None                            
            }
          }
          Constraint.evalConstraint(rule.constraint, f) match {
            case Some(eval) => 
              coreItem.evalConstraint = { 
                case (param, layout, results) => 
                  eval(param, layout, results) match {
                    case Some(q) => q
                    case None => true
                  }
              }
            case _ =>
          }
          coreItem.evalParam = evalParam
          coreItem.evalResult = evalResult
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

  val (numScopes, scopeOfTerminal, lexerOfTerminal) = {
    var scopes : Map[String, Int] = Map()
    val scopesOfTerminals : Array[Int] = new Array(terminalOfId.size)
    val lexers = new Array[Lexer](terminalOfId.size)
    var scope = 0
    for (terminal <- grammar.terminals) {
      val scanrule = grammar.scanrules(terminal)
      val terminalId = idOfTerminal(terminal)
      val terminalIndex = (-terminalId) - 1
      val entry = (terminalId, scanrule.lexer)
      scopes.get(scanrule.scope) match {
        case None => 
          scopes += (scanrule.scope -> scope)
          scopesOfTerminals(terminalIndex) = scope
          scope += 1
        case Some(scope) => 
          scopesOfTerminals(terminalIndex) = scope
      }
      lexers(terminalIndex) = scanrule.lexer
    }
    def scopeOfTerminal(terminalId : Int) : Int = {
      scopesOfTerminals((-terminalId) - 1)
    }
    def lexerOfTerminal(terminalId : Int) : Lexer = {
      lexers((-terminalId) - 1)
    }
    (scopes.size, scopeOfTerminal _, lexerOfTerminal _)
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

  def prioritizeTerminalsWithParams(terminalsWithParams : Set[(Int, ParseParam.V)]) : Set[(Int, ParseParam.V)] = {
    val terminals = terminalsWithParams.map(x => x._1)
    val prioritizedTerminals = prioritizeTerminals(terminals)
    terminalsWithParams.filter(t => prioritizedTerminals.contains(t._1))
  }

  def coreItemOf(item : Earley.Item) : CoreItem = coreItems(item.coreItemId)

  def stringOfId(id : Int) : String = {
    if (id < 0) terminalOfId(id) else nonterminalOfId(id)
  }

  def coreItem2String(coreItem : CoreItem) : String = {
    var s = ""
    s = s + stringOfId(coreItem.nonterminal) + " => "
    var i = 0
    while (i < coreItem.rhs.length) {
      if (i == coreItem.dot) s = s + ". "
      s = s + stringOfId(coreItem.rhs(i)) + " "
      i = i + 1
    }
    if (i == coreItem.dot) s = s + ". "
    s
  }

  def nullresult(nonterminalId : Int) : ParseParam.V = Earley.DEFAULT_RESULT

}