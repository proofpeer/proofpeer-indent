package proofpeer.indent.earley

import proofpeer.indent._
import proofpeer.indent.regex.DFA
import proofpeer.indent.regex.DocumentCharacterStream

object HyperEarley {

  final class Item(val hyperCoreItemId : Int, val origin : Int, val layout : Span.Layout, val nextSibling : Item, 
    var nextItem : Item)

}

final class HyperBitmapPool(numItems : Int) {

  type Bitmap = Array[HyperEarley.Item]

  private var bitmaps : List[Bitmap] = List()

  def allocate() : Bitmap = {
    bitmaps match {
      case bitmap :: bs =>
        bitmaps = bs
        bitmap
      case _ =>
        new Array(numItems)
    }
  }

  def release(bitmap : Bitmap) {
    for (i <- 0 until numItems) bitmap(i) = null
    bitmaps = bitmap :: bitmaps
  }

}

final class HyperBin(val pool : HyperBitmapPool) {

  import HyperEarley._

  var processedItems : Item = null

  private var newItems : Item = null 

  private var bitmap = pool.allocate()

  def countItems(item : Item) : Int = {
    var num = 0
    var x = item
    while (x != null) {
      num += 1
      x = x.nextItem
    }
    num
  }

  def size : Int = countItems(newItems) + countItems(processedItems)

  def nextItem() : Item = {
    if (newItems != null) {
      val item = newItems
      newItems = item.nextItem
      item.nextItem = processedItems
      processedItems = item
      item
    } else null
  }

  def addItem(itemId : Int, origin : Int, layout : Span.Layout) {
    var item = bitmap(itemId)
    if (item == null) {
      newItems = new Item(itemId, origin, layout, null, newItems)
      bitmap(itemId) = newItems
    } else if (item.origin != origin || !Span.layoutsAreEqual(item.layout, layout)) {
      var sibling = item.nextSibling
      while (sibling != null && (origin != sibling.origin || !Span.layoutsAreEqual(layout, sibling.layout)))
        sibling = sibling.nextSibling
      if (sibling == null) {
        newItems = new Item(itemId, origin, layout, item, newItems)
        bitmap(itemId) = newItems
      }
    }
  }

  /** This is called when no new calls to [[addItem]] will be made. */
  def finishedAdding() {
    pool.release(bitmap)
    bitmap = null
  }

}

final class HyperEarley(hea : HyperEarleyAutomaton) {

  import HyperEarley._

  val pool = new HyperBitmapPool(hea.hyperCoreItems.size)
  
  def initialBin : HyperBin = {
    val bin = new HyperBin(pool)
    for ((hyperCoreItemId, layout) <- hea.startStates) {
      bin.addItem(hyperCoreItemId, 0, layout.realize(null))
    }
    bin
  }

  def predictAndComplete(bins : Array[HyperBin], k : Int) : scala.collection.mutable.Set[Int] = {
    val bin = bins(k)
    if (bin == null) return null
    var item = bin.nextItem()
    var terminals : scala.collection.mutable.Set[Int] = scala.collection.mutable.Set()
    while (item != null) {
      val hyperCoreItem = hea.hyperCoreItems(item.hyperCoreItemId)
      terminals ++= hyperCoreItem.terminals
      if (hyperCoreItem.completedNonterminals != null) {
        val span = Span.spanOfLayout(item.layout)
        val parentItem = bins(item.origin).processedItems
        var completedNonterminal = hyperCoreItem.completedNonterminals
        do {
          if (completedNonterminal.unconstrained || completedNonterminal.coreItems.exists(c => c.evalConstraint(item.layout))) {
            var originItem = parentItem
            val nonterminal = completedNonterminal.nonterminal
            while (originItem != null) {
              val originHyperCoreItem = hea.hyperCoreItems(originItem.hyperCoreItemId)
              val action = originHyperCoreItem.nonterminalTransitions(nonterminal - 1)
              if (action != null) {
                val layout = Span.addToLayout(originItem.layout, span)
                action(bin, originItem.origin, k, layout)
              }
              originItem = originItem.nextItem
            }
          } 
          completedNonterminal = completedNonterminal.next
        } while (completedNonterminal != null)
      }
      item = bin.nextItem()
    }    
    bin.finishedAdding()
    terminals 
  }

  def scan(document : Document, stream : DocumentCharacterStream, bins : Array[HyperBin], k : Int, terminals : scala.collection.mutable.Set[Int]) {
    if (terminals == null || terminals.isEmpty) return
    var scopes : Map[Int, Set[Int]] = Map()
    for (terminal <- terminals) {
      val scope = hea.ea.scopeOfTerminal(terminal)
      scopes.get(scope) match {
        case None => scopes += (scope -> Set(terminal))
        case Some(ts) => scopes += (scope -> (ts + terminal))
      }
    }
    val (row, column, _) = document.character(k)
    val (_, column0, _) = document.character(document.firstPositionInRow(row))
    for ((scope, terminals) <- scopes) {
      val dfa = hea.ea.dfas(scope)
      stream.setPosition(k)
      val (len, _recognizedTerminals) = DFA.run(dfa, stream, terminals)
      val recognizedTerminals = hea.ea.prioritizeTerminals(_recognizedTerminals)
      if (recognizedTerminals != null && !recognizedTerminals.isEmpty) {
        val span = Span(column0, row, column, k, len)
        var item = bins(k).processedItems
        var destBin = bins(k + len)
        if (destBin == null) {
          destBin = new HyperBin(pool)
          bins(k + len) = destBin
        }
        while (item != null) {
          val hyperCoreItem = hea.hyperCoreItems(item.hyperCoreItemId)
          for (terminal <- hyperCoreItem.terminals) {
            if (recognizedTerminals.contains(terminal)) {
              val layout = Span.addToLayout(item.layout, span)
              val action = hyperCoreItem.terminalTransitions(- terminal - 1)
              action(destBin, item.origin, k + len, layout)
            }
          }
          item = item.nextItem
        }
      }
    } 
  }

  def parsedCoreItems(bin : HyperBin, nonterminal : Int, origin : Int) : List[(Item, CoreItem)] = {
    if (bin == null) return List()
    var items : List[(Item, CoreItem)] = List()
    var item = bin.processedItems
    while (item != null) {
      if (item.origin == origin) {
        val hyperCoreItem = hea.hyperCoreItems(item.hyperCoreItemId)
        var completedNonterminal = hyperCoreItem.completedNonterminals
        while (completedNonterminal != null && completedNonterminal.nonterminal != nonterminal)
          completedNonterminal = completedNonterminal.next
        if (completedNonterminal != null) {
          val cs = 
            if (completedNonterminal.unconstrained)
              completedNonterminal.coreItems
            else 
              completedNonterminal.coreItems.filter(c => c.evalConstraint(item.layout))
          items ++= cs.map(x => (item, x))
        }
      }
      item = item.nextItem
    }
    items
  }

  def recognizedNonterminals(bin : HyperBin) : Set[Int] = {
    if (bin == null) return Set()
    var nonterminals : Set[Int] = Set()
    var item = bin.processedItems
    while (item != null) {
      if (item.origin == 0) {
        val hyperCoreItem = hea.hyperCoreItems(item.hyperCoreItemId)
        var completedNonterminal = hyperCoreItem.completedNonterminals
        while (completedNonterminal != null) {
          if (completedNonterminal.unconstrained ||
            completedNonterminal.coreItems.exists(c => c.evalConstraint(item.layout)))
          {
            nonterminals += completedNonterminal.nonterminal 
          }
          completedNonterminal = completedNonterminal.next
        }
      }
      item = item.nextItem
    }
    nonterminals    
  }

  def recognize(document : Document, nonterminals : Set[Int]) : Either[(Set[Int], Array[HyperBin]), Int]  = {
    var bins : Array[HyperBin] = new Array(document.size + 1)
    bins(0) = initialBin
    val stream = new DocumentCharacterStream(document)
    for (k <- 0 until document.size) {
      scan(document, stream, bins, k, predictAndComplete(bins, k))
    }
    predictAndComplete(bins, document.size)
    val recognized = recognizedNonterminals(bins(document.size)).intersect(nonterminals)
    if (recognized.isEmpty) {
      var k = document.size
      var foundNonemptyBin = false
      while (k >= 0 && !foundNonemptyBin) {
        if (bins(k) != null && bins(k).processedItems != null) 
          foundNonemptyBin = true
        else k -= 1
      }
      Right(k)
    } else {
      Left((recognized, bins))
    } 
  }

  /** Constructs the parse tree using the information obtained from the recognition phase. This assumes that there actually exists at least one parse tree.
    * @param startPosition the start position (inclusive)
    * @param endPosition the end position (exclusive)
    */    
  def constructParseTree(document : Document, bins : Array[HyperBin], nonterminal : Int, startPosition : Int, endPosition : Int) : ParseTree = {
    val grammar = hea.ea.grammar
    val nonterminalSymbol = hea.ea.nonterminalOfId(nonterminal)
    val bin = bins(endPosition)
    val items = parsedCoreItems(bin, nonterminal, startPosition)
    var foundItem : Item = null
    var foundCoreItem : CoreItem = null
    for ((item, coreItem) <- items) {
      if (foundItem != null && (coreItem.ruleindex != foundCoreItem.ruleindex || !Span.layoutsAreEqual(foundItem.layout, item.layout))) 
        return AmbiguousNode(nonterminalSymbol, Span.spanOfLayout(foundItem.layout))
      foundItem = item
      foundCoreItem = coreItem
    }
    if (foundItem == null) throw new RuntimeException("cannot construct parse tree")
    val coreItem = foundCoreItem
    var subtrees = new Array[ParseTree](coreItem.rhs.size)
    var pos = startPosition
    for (i <- 0 until subtrees.size) {
      val symbol = coreItem.rhs(i)
      val span = foundItem.layout(i)
      if (symbol < 0) {
        subtrees(i) = TerminalNode(hea.ea.terminalOfId(symbol), span)
        pos = span.lastTokenIndex + 1
      } else if (span != null) {
        subtrees(i) = constructParseTree(document, bins, symbol, span.firstTokenIndex, span.lastTokenIndex + 1)
        pos = span.lastTokenIndex + 1
      } else
        subtrees(i) = constructParseTree(document, bins, symbol, pos, pos) 
    }
    val ruleindex = coreItem.ruleindex
    val parserule = grammar.parserules(nonterminalSymbol)(ruleindex)
    val rhsIndices = grammar.rhsIndices(nonterminalSymbol, ruleindex)
    val span_ = Span.spanOfLayout(foundItem.layout)
    val d_ = document
    val g_ = grammar
    val sp_ = startPosition
    val ep_ = endPosition
    class Context extends ParseContext {
      val document = d_
      val grammar = g_
      val rule = parserule
      val span = span_
      val startPosition = sp_
      val endPosition = ep_ 
      def result(indexedSymbol : IndexedSymbol) = subtrees(rhsIndices(indexedSymbol))           
    }
    val value = parserule.action(new Context())
    NonterminalNode(nonterminalSymbol, ruleindex, span_, subtrees.toVector, value)
  }

  def parse(document : Document, nonterminalSymbol : String) : Either[ParseTree, Int] = {
    val nonterminal = hea.ea.idOfNonterminal(nonterminalSymbol)
    recognize(document, Set(nonterminal)) match {
      case Left((recognized, bins)) =>
        Left(constructParseTree(document, bins, nonterminal, 0, document.size))
      case Right(k) => 
        Right(k) 
    }
  } 

}