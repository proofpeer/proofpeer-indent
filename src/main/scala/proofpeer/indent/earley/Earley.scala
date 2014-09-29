package proofpeer.indent.earley

import proofpeer.indent._
import proofpeer.indent.regex.DFA
import proofpeer.indent.regex.DocumentCharacterStream

object Earley {

  final class Item(val coreItemId : Int, val origin : Int, val layout : Span.Layout, val nextSibling : Item, var nextItem : Item)

}

sealed trait ParseTree {
  def symbol : Int
  def span : Span
  def hasAmbiguities : Boolean
}

final case class AmbiguousNode(nonterminal : Int, span : Span) extends ParseTree {
  def symbol = nonterminal
  def hasAmbiguities = true
}

final case class NonterminalNode(nonterminal : Int, ruleindex : Int, span : Span, rhs : Vector[ParseTree]) extends ParseTree {
  def symbol = nonterminal
  def hasAmbiguities = rhs.exists(_.hasAmbiguities)
}

final case class TerminalNode(terminal : Int, span : Span) extends ParseTree {
  def symbol = terminal
  def hasAmbiguities = false
}

final class BitmapPool(numCoreItems : Int) {

  type Bitmap = Array[Earley.Item]

  private var bitmaps : List[Bitmap] = List()

  def allocate() : Bitmap = {
    bitmaps match {
      case bitmap :: bs =>
        bitmaps = bs
        bitmap
      case _ =>
        new Array(numCoreItems)
    }
  }

  def release(bitmap : Bitmap) {
    for (i <- 0 until numCoreItems) bitmap(i) = null
    bitmaps = bitmap :: bitmaps
  }

}

final class Bin(val pool : BitmapPool) {

  import Earley._

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

  def addItem(coreItemId : Int, origin : Int, layout : Span.Layout) {
    Measuring.addItem += 1
    var item = bitmap(coreItemId)
    if (item == null) {
      newItems = new Item(coreItemId, origin, layout, null, newItems)
      bitmap(coreItemId) = newItems
    } else if (item.origin != origin || !Span.layoutsAreEqual(item.layout, layout)) {
      var sibling = item.nextSibling
      while (sibling != null && (origin != sibling.origin || !Span.layoutsAreEqual(layout, sibling.layout)))
        sibling = sibling.nextSibling
      if (sibling == null) {
        newItems = new Item(coreItemId, origin, layout, item, newItems)
        bitmap(coreItemId) = newItems
      }
    }
  }

  /** This is called when no new calls to [[addItem]] will be made. */
  def finishedAdding() {
    pool.release(bitmap)
    bitmap = null
  }

}

final class Earley(ea : EarleyAutomaton) {

  import Earley._

  val pool = new BitmapPool(ea.coreItems.size)
  
  def initialBin(nonterminals : Set[Int]) : Bin = {
    val bin = new Bin(pool)
    for (coreItemId <- 0 until ea.coreItems.size) {
      val coreItem = ea.coreItems(coreItemId)
      if (coreItem.dot == 0 && nonterminals.contains(coreItem.nonterminal)) 
        bin.addItem(coreItemId, 0, null)
    }
    bin
  }

  def predictAndComplete(bins : Array[Bin], k : Int) : Set[Int] = {
    val bin = bins(k)
    if (bin == null) return null
    var item = bin.nextItem()
    var terminals : Set[Int] = Set()
    while (item != null) {
      val coreItem = ea.coreItemOf(item)
      val nextSymbol = coreItem.nextSymbol
      if (nextSymbol < 0) /* terminal */ 
        terminals += nextSymbol
      else if (nextSymbol > 0) /* nonterminal */ {
        for (predictedItem <- coreItem.predictedCoreItems) 
          bin.addItem(predictedItem, k, null)
        if (coreItem.nextSymbolIsNullable) 
          bin.addItem(coreItem.nextCoreItem, item.origin, Span.addToLayout(item.layout, null))
      } else if (coreItem.dot > 0) /* no symbol, do completion for non-epsilon rules */ {
        val nonterminal = coreItem.nonterminal
        val span = Span.spanOfLayout(item.layout)
        var originItem = bins(item.origin).processedItems
        while (originItem != null) {
          val originCoreItem = ea.coreItemOf(originItem)
          if (originCoreItem.nextSymbol == nonterminal) {
            val layout = Span.addToLayout(originItem.layout, span)
            val nextCoreItemId = originCoreItem.nextCoreItem
            val nextCoreItem = ea.coreItems(nextCoreItemId)
            if (nextCoreItem.evalConstraint(layout)) 
              bin.addItem(nextCoreItemId, originItem.origin, layout)
          }
          originItem = originItem.nextItem
        }
      } 
      item = bin.nextItem()
    }    
    bin.finishedAdding()
    terminals
  }

  def scan(document : Document, stream : DocumentCharacterStream, bins : Array[Bin], k : Int, terminals : Set[Int]) {
    if (terminals == null || terminals.isEmpty) return
    var scopes : Map[Int, Set[Int]] = Map()
    for (terminal <- terminals) {
      val scope = ea.scopeOfTerminal(terminal)
      scopes.get(scope) match {
        case None => scopes += (scope -> Set(terminal))
        case Some(ts) => scopes += (scope -> (ts + terminal))
      }
    }
    val (row, column, _) = document.character(k)
    val (_, column0, _) = document.character(document.firstPositionInRow(row))
    for ((scope, terminals) <- scopes) {
      val dfa = ea.dfas(scope)
      stream.setPosition(k)
      val (len, _recognizedTerminals) = DFA.run(dfa, stream, terminals)
      val recognizedTerminals = ea.prioritizeTerminals(_recognizedTerminals)
      if (recognizedTerminals != null && !recognizedTerminals.isEmpty) {
        val span = Span(column0, row, column, k, len)
        var item = bins(k).processedItems
        var destBin = bins(k + len)
        if (destBin == null) {
          destBin = new Bin(pool)
          bins(k + len) = destBin
        }
        while (item != null) {
          val coreItem = ea.coreItemOf(item)
          if (coreItem.nextSymbol < 0 && recognizedTerminals.contains(coreItem.nextSymbol)) {
            val layout = Span.addToLayout(item.layout, span)
            val nextCoreItem = ea.coreItems(coreItem.nextCoreItem)
            if (nextCoreItem.evalConstraint(layout)) {
              destBin.addItem(coreItem.nextCoreItem, item.origin, layout)
            }
          }
          item = item.nextItem
        }
      }
    }
  }

  def recognizedNonterminals(bin : Bin) : Set[Int] = {
    if (bin == null) return Set()
    var recognized : Set[Int] = Set()
    var item = bin.processedItems
    while (item != null) {
      if (item.origin == 0) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nextSymbol == 0) recognized += coreItem.nonterminal
      }
      item = item.nextItem
    }
    recognized
  }

  def recognize(document : Document, nonterminals : Set[Int]) : Either[(Set[Int], Array[Bin]), Int]  = {
    var bins : Array[Bin] = new Array(document.size + 1)
    bins(0) = initialBin(nonterminals)
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
  def constructParseTree(document : Document, bins : Array[Bin], nonterminal : Int, startPosition : Int, endPosition : Int) : ParseTree = {
    val bin = bins(endPosition)
    var item = bin.processedItems
    var foundItem : Item = null
    while (item != null) {
      val coreItem = ea.coreItemOf(item)
      if (coreItem.nonterminal == nonterminal && coreItem.nextSymbol == 0 && item.origin == startPosition) {
        if (foundItem != null) return AmbiguousNode(nonterminal, Span.spanOfLayout(foundItem.layout))
        foundItem = item
      }
      item = item.nextItem
    }
    if (foundItem == null) throw new RuntimeException("cannot construct parse tree")
    val coreItem = ea.coreItemOf(foundItem)
    var subtrees = new Array[ParseTree](coreItem.rhs.size)
    var pos = startPosition
    for (i <- 0 until subtrees.size) {
      val symbol = coreItem.rhs(i)
      val span = foundItem.layout(i)
      if (symbol < 0) {
        subtrees(i) = TerminalNode(symbol, span)
        pos = span.lastTokenIndex + 1
      } else if (span != null) {
        subtrees(i) = constructParseTree(document, bins, symbol, span.firstTokenIndex, span.lastTokenIndex + 1)
        pos = span.lastTokenIndex + 1
      } else
        subtrees(i) = constructParseTree(document, bins, symbol, pos, pos) 
    }
    NonterminalNode(nonterminal, coreItem.ruleindex, Span.spanOfLayout(foundItem.layout), subtrees.toVector)
  }

  def parse(document : Document, nonterminal : Int) : Either[ParseTree, Int] = {
    recognize(document, Set(nonterminal)) match {
      case Left((recognized, bins)) =>
        Left(constructParseTree(document, bins, nonterminal, 0, document.size))
      case Right(k) => 
        Right(k) 
    }
  } 

}