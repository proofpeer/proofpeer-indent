package proofpeer.indent.earley

import proofpeer.indent.API._
import scala.collection.immutable._
import scala.collection.mutable.{Map => MutableMap}

case class Item(nonterminal : Nonterminal, ruleindex : Int, dot : Int, origin : Int)

class Earley[Value, IntermediateValue, BlackboxValue](
    grammar : BlackboxGrammar[Value, IntermediateValue, BlackboxValue], 
    document : Document) 
{
  
  def ruleOfItem(item : Item) : Rule[Int] = grammar.rulesOfNonterminal(item.nonterminal)(item.ruleindex)
  
  def isCompletedItem(item : Item) : Boolean = {
    val rule = ruleOfItem(item)
    item.dot == rule.rhs.size
  }
  
  def expectedSymbol(item : Item) : Option[Symbol] = {
    val rule = ruleOfItem(item)
    val rhs = rule.rhs
    val dot = item.dot
    if (dot < rhs.size) Some(rhs(dot).indexedSymbol.symbol) else None
  }
  
  type MoreItems = Seq[(Item, IntermediateValue)]
  
  case class ItemBin(binindex : Int, items : MutableMap[Item, IntermediateValue]) {

    /** Add item -> value to the bin.
      * @return true if adding item -> value changes then bin, otherwise false 
      */
    def add(item : Item, value : IntermediateValue) : Boolean = {
      items.get(item) match {
        case None => 
          items += (item -> value)
          true
        case Some (oldvalue) =>
          grammar.mergeValues(document, item.origin, binindex, item.nonterminal, item.ruleindex, 
            item.dot, oldvalue, value) match 
          {
            case None =>
              false
            case Some(newvalue) =>
              items += (item -> newvalue)
              true
          }
      }
    }
    
    def add(more_items : MoreItems) : Boolean = {
      var changed = false
      for ((item, value) <- more_items) {
        changed = add(item, value) || changed
      }
      changed
    }
    
  }
  
  object ItemBin {
    
    def empty(index : Int) = new ItemBin(index, MutableMap())
    
    def singleton(index : Int, item : Item, value : IntermediateValue) = new ItemBin(index, MutableMap(item -> value))
    
  }
  
  def predict_items(nonterminal : Nonterminal, origin : Int) : MoreItems = {
    val rules = grammar.rulesOfNonterminal(nonterminal)
    var ruleindex = 0
    rules.toSeq.map {
      case rule =>
        val item = Item(nonterminal, ruleindex, 0, origin)
        val value = grammar.initialValue(document, origin, nonterminal, ruleindex)
        ruleindex += 1
        (item -> value)
    }
  }
  
  def complete_items(completed_binindex : Int, bin : ItemBin, nonterminal : Nonterminal, value : Value) : MoreItems = {
    var completed_items : List[(Item, IntermediateValue)] = List()
    for ((item, v) <- bin.items) {
      val s = expectedSymbol(item)
      if (s == Some (nonterminal)) {
        grammar.nextValue(document, item.origin, bin.binindex, completed_binindex, nonterminal, 
          item.ruleindex, item.dot, v, value) match 
        {
          case None => 
          case Some(nextvalue) =>
            completed_items = (Item(item.nonterminal, item.ruleindex, item.dot + 1, item.origin) -> nextvalue) :: 
              completed_items
        }
      }
    }
    completed_items    
  } 
  
}