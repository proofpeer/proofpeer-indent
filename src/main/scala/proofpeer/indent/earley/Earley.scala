package proofpeer.indent.earley

import proofpeer.indent.API.{Terminal, Nonterminal, TerminalLike, Symbol}
import proofpeer.indent.Document

import scala.collection.immutable._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{Set => MutableSet}

case class Item(nonterminal : Nonterminal, ruleindex : Int, dot : Int, origin : Int) {
  def inc : Item = Item(nonterminal, ruleindex, dot + 1, origin)
  //override def hashCode : Int = List[Any](nonterminal, ruleindex, dot, origin).hashCode
}

/** General-purpose Earley parser. Apart from using Terminal, Nonterminal, TerminalLike
  * and Symbol, it is entirely independent of [[proofpeer.indent.API]].
  * It can be used in any situation that calls for an Earley parser / recognizer. 
  * This implementation has several special features compared with the usual Earley recognizer:
  *   - It computes values and can therefore be used not only for recognizing, but also for parsing
  *   - It includes the parsing of blackboxes: the parser can delegate parsing of 
  *     blackbox nonterminals to third-parties.
  *   - The application of rules can be rejected based on the values that have been computed for the 
  *     symbols on the right hand side. 
  */
class Earley[Value, IntermediateValue](
    grammar : BlackboxGrammar[Value, IntermediateValue], 
    document : Document) 
{
  
  def ruleOfItem(item : Item) : Rule = grammar.rulesOfNonterminal(item.nonterminal)(item.ruleindex)
  
  def isCompletedItem(item : Item) : Boolean = {
    val rule = ruleOfItem(item)
    item.dot == rule.rhsSize
  }
  
  def expectedSymbol(item : Item) : Option[Symbol] = {
    val rule = ruleOfItem(item)
    val dot = item.dot
    if (dot < rule.rhsSize) Some(rule.rhsAt(dot)) else None
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
        grammar.nextValue(document, item.origin, bin.binindex, completed_binindex, 
          item.nonterminal, item.ruleindex, item.dot, v, value) match 
        {
          case None => 
          case Some(nextvalue) =>
            completed_items = (item.inc -> nextvalue) :: completed_items
        }
      }
    }
    completed_items    
  }
    
  class Bins {
    private val bins : MutableMap[Int, ItemBin] = MutableMap()
    def apply(k : Int) : ItemBin = {
      bins.get(k) match {
        case Some(bin) => bin
        case None =>
          val bin = ItemBin.empty(k)
          bins += (k -> bin)
          bin
      }
    }
  }
  
  def predict_and_complete_step(bins : Bins, k : Int)  = {
    var changed = false
    val bin = bins(k)
    do {
      changed = false
      for ((item, value) <- bin.items) {
        val expected = expectedSymbol(item)
        val predicted_items : MoreItems = 
          expected match {
            case Some(n : Nonterminal) => predict_items(n, k)
            case _ => List()
          }
        val completed_items : MoreItems =
          expected match {
            case None =>
              val finalvalue = grammar.finalValue(document, item.origin, k, 
                item.nonterminal, item.ruleindex, value)
              complete_items(k, bins(item.origin), item.nonterminal, finalvalue)
            case _ => List()
          }
        changed = bin.add(predicted_items) || changed
        changed = bin.add(completed_items) || changed
      }
    } while (changed)
  }
  
  def scan_step(bins : Bins, k : Int) : Boolean = {
    if (k >= document.size) return false
    val nextToken = document.getToken(k)
    val nextTerminal = nextToken.terminal
    val nextTokenValue = grammar.valueOfToken(nextToken)
    val bin = bins(k + 1)
    var changed = false
    for ((item, value) <- bins(k).items) {
      expectedSymbol(item) match {
        case Some(expected : TerminalLike) if expected.isLike(nextTerminal) =>
          val nextvalue = grammar.nextValue(document, item.origin, k, k+1, 
            item.nonterminal, item.ruleindex, item.dot, value, nextTokenValue)
          nextvalue match {
            case Some(nextvalue) => 
              changed = bin.add(item.inc, nextvalue) || changed
            case _ =>
          }
        case _ => None
      }
    }
    changed    
  }
  
  def collect_blackboxes(bin : ItemBin) : Set[Nonterminal] = {
    var blackboxes : Set[Nonterminal] = Set()
    for ((item, _) <- bin.items) {
      expectedSymbol(item) match {
        case Some(n : Nonterminal) if grammar.isBlackbox(n) =>
          blackboxes += n
        case _ =>
      }  
    }
    blackboxes
  }
  
  def blackbox_step(bins : Bins, k : Int) : (Boolean, Int) = {
    if (!grammar.hasBlackboxes) return (false, k)
    val bin = bins(k)
    val blackboxes = collect_blackboxes(bin)
    if (blackboxes.isEmpty) return (false, k)
    val results = grammar.callBlackboxes(document, k, document.size, blackboxes)
    if (results.isEmpty) return (false, k)
    var largest_k = k
    var changed = false    
    for ((item, value) <- bin.items) {
      expectedSymbol(item) match {
        case Some(blackbox : Nonterminal) =>
          results.get(blackbox) match {
            case Some(parses) =>
              for ((j, v) <- parses) {
                grammar.nextValue(document, item.origin, k, j, 
                  item.nonterminal, item.ruleindex, item.dot, value, v) match
                {
                  case Some(nextvalue) =>
                    if (bins(j).add(item.inc, nextvalue)) {
                      changed = changed || (j == k)
                      if (j > largest_k) largest_k = j
                    }
                  case _ =>
                }
              }
            case _ =>
          }
        case _ =>          
      }
    }
    (changed, largest_k)
  }
  
  def extend_items(bins : Bins, k : Int) : Int = {
    var largest_k = k
    do {
      predict_and_complete_step(bins, k)
      val (changed, j) = blackbox_step(bins, k)
      if (j > largest_k) largest_k = j
      if (!changed) {
        if (scan_step(bins, k) && k + 1 > largest_k) largest_k = k + 1        
        return largest_k
      }
    } while (true)
    return largest_k // make your loving compiler happy
  }
  
  def initialBins(nonterminals : Set[Nonterminal], k : Int) : Bins = {
    val bins = new Bins()
    val bin = bins(k)
    for (nonterminal <- nonterminals) {
      bin.add(predict_items(nonterminal, k))
    }
    bins
  }
  
  def recognize(nonterminals : Set[Nonterminal], i : Int) : (Bins, Int) = {
    val bins = initialBins(nonterminals, i)
    var largest_k = i
    var k = i
    while (k <= largest_k) {
      val j = extend_items(bins, k)
      if (j > largest_k) largest_k = j
      k = k + 1
    }
    (bins, largest_k)
  }
  
  def compute_parse_values(nonterminals : Nonterminal => Boolean, bins : Bins, i : Int, j : Int) : 
    Map[Nonterminal, Value] = 
  {
    val bin = bins(j)
    var result : Map[Nonterminal, Value] = Map()
    for ((item, intermediateValue) <- bin.items) {
      if (item.origin == i && nonterminals(item.nonterminal) && isCompletedItem(item)) {
        val n = item.nonterminal
        val value = grammar.finalValue(document, i, j, n, item.ruleindex, intermediateValue)
        result.get(n) match {
          case None => result += (n -> value)
          case Some(v) => result += (n -> grammar.joinValues(document, i, j, n, v, value))
        }
      }
    }
    result
  }
  
  def compute_longest_parse_values(nonterminals : Nonterminal => Boolean, bins : Bins, i : Int, j : Int) :
    (Map[Nonterminal, Value], Int) =
  {
    var k = j
    while (i <= k) {
      val result = compute_parse_values(nonterminals, bins, i, k)
      if (!result.isEmpty) return (result, k)
      k = k - 1
    }
    return (Map(), k)
  }
  
  def parse(nonterminal : Nonterminal, k : Int) : Option[(Value, Int)] = {
    val (bins, largest_k) = recognize(Set(nonterminal), k)
    val (result, i) = compute_longest_parse_values(n => n == nonterminal, bins, k, largest_k)
    result.get(nonterminal) match {
      case None => None
      case Some(value) => Some(value, i)
    }
  }
  
}