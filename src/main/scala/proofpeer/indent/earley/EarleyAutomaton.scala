package proofpeer.indent.earley

import proofpeer.indent._

case class StateDescr(val id : Int, val ruleindex : Int, val dot : Int) 

class EarleyAutomaton(grammar : Grammar) {

  val states : Map[Int, StateDescr] = {
    var states : Map[Int, StateDescr] = Map()
    var ruleindex = 0
    for (rule <- grammar.rules) {
      rule match {
        case rule : ScanRule =>
        case rule : ParseRule =>
          for (dot <- 0 to rule.rhs.size) {
            val id = states.size
            states += (id -> StateDescr(id, ruleindex, dot))
          }
      }
      ruleindex += 1
    }
    states
  }

}