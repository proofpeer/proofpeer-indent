package proofpeer.indent.earley

import proofpeer.indent._
import proofpeer.indent.regex.DFA
import proofpeer.indent.regex.DocumentCharacterStream

object Earley {

  var debug = false

  final val DEFAULT_PARAM = -1

  final class Item(val coreItemId : Int, val param : Int, val origin : Int, val layout : Span.Layout, val nextSibling : Item, var nextItem : Item) {
    override def toString : String = {
      val p = if (param == DEFAULT_PARAM) "" else "{" + param + "}"
      "Earley.Item[coreItemId="+coreItemId+p+", origin="+origin+", layout="+layout+"]"
    }
  }

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

  var newItems : Item = null 

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

  def addItem(coreItemId : Int, param : Int, origin : Int, layout : Span.Layout) {
    var item = bitmap(coreItemId)
    if (item == null) {
      newItems = new Item(coreItemId, param, origin, layout, null, newItems)
      bitmap(coreItemId) = newItems
    } else if (item.param != param || item.origin != origin || 
      !Span.layoutsAreEqual(item.layout, layout)) 
    {
      var sibling = item.nextSibling
      while (sibling != null && (param != sibling.param || origin != sibling.origin 
        || !Span.layoutsAreEqual(layout, sibling.layout)))
        sibling = sibling.nextSibling
      if (sibling == null) {
        newItems = new Item(coreItemId, param, origin, layout, item, newItems)
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
        bin.addItem(coreItemId, DEFAULT_PARAM, 0, Span.emptyLayout(0, coreItem.includes))
    }
    bin
  }

  def predictAndComplete(bins : Array[Bin], k : Int) : Set[(Int, Int)] = {
    val bin = bins(k)
    if (bin == null) return null
    var item = bin.nextItem()
    var terminals : Set[(Int, Int)] = Set()
    while (item != null) {
      val coreItem = ea.coreItemOf(item)
      val param = item.param
      val nextSymbol = coreItem.nextSymbol
      if (nextSymbol < 0) /* terminal */ {
        val nextSymbolParam = coreItem.evalParam(param, item.layout, coreItem.dot)
        terminals += (nextSymbol -> nextSymbolParam)
      } else if (nextSymbol > 0) /* nonterminal */ {
        val nextSymbolParam = coreItem.evalParam(param, item.layout, coreItem.dot)
        for (predictedItem <- coreItem.predictedCoreItems) {
          val predictedCoreItem = ea.coreItems(predictedItem)
          val layout = Span.emptyLayout(k, predictedCoreItem.includes)
          bin.addItem(predictedItem, nextSymbolParam, k, layout)
        }
        if (coreItem.nextSymbolIsNullable) {
          val layout = Span.addToLayout(item.origin, coreItem.includes, item.layout, 
            Span.nullSpan(k, k))
          val nextCoreItemId = coreItem.nextCoreItem
          val nextCoreItem = ea.coreItems(nextCoreItemId)
          if (nextCoreItem.evalConstraint(param, layout))
            bin.addItem(nextCoreItemId, param, item.origin, layout)
        }
      } else if (coreItem.dot > 0) /* no symbol, do completion for non-epsilon rules */ {
        val nonterminal = coreItem.nonterminal
        val span = Span.getSpanOfLayout(item.layout)
        var originItem = bins(item.origin).processedItems
        while (originItem != null) {
          val originCoreItem = ea.coreItemOf(originItem)
          if (originCoreItem.nextSymbol == nonterminal) {
            val p = originCoreItem.evalParam(originItem.param, originItem.layout, originCoreItem.dot)
            if (p == param) {
              val layout = Span.addToLayout(originItem.origin, originCoreItem.includes,
                originItem.layout, span)
              val nextCoreItemId = originCoreItem.nextCoreItem
              val nextCoreItem = ea.coreItems(nextCoreItemId)
              if (nextCoreItem.evalConstraint(param, layout)) 
                bin.addItem(nextCoreItemId, param, originItem.origin, layout)
            }
          }
          originItem = originItem.nextItem
        }
      } 
      item = bin.nextItem()
    }    
    bin.finishedAdding()
    terminals
  }

  def scan(document : Document, bins : Array[Bin], k : Int, terminals : Set[(Int, Int)]) {
    if (terminals == null || terminals.isEmpty) return

    import scala.collection.mutable.{Map => MutableMap}
    
    // check which terminals actually can be scanned from position k on
    var scans : MutableMap[(Int, Int), Int] = MutableMap()
    for (t <- terminals) {
      val lexer = ea.lexerOfTerminal(t._1)
      val len = lexer.lex(document, k, t._2)
      if (len > 0) scans += (t -> len)
    }

    // check which scans are compatible with some layout 
    var scopes : MutableMap[Int, (Int, Set[(Int, Int)])] = MutableMap()
    val (row, column, _) = document.character(k)
    val (_, column0, _) = document.character(document.firstPositionInRow(row))
    var item = bins(k).processedItems
    while (item != null) {
      val coreItem = ea.coreItemOf(item)
      if (coreItem.nextSymbol < 0) {
        val terminal = coreItem.nextSymbol
        val terminalParam = coreItem.evalParam(item.param, item.layout, coreItem.dot)
        val t = (terminal -> terminalParam)
        scans.get(t) match {
          case None =>
          case Some(len) =>
            val span = Span(column0, row, column, k, len) 
            val layout = Span.addToLayout(item.origin, coreItem.includes, item.layout, span)
            val nextCoreItem = ea.coreItems(coreItem.nextCoreItem)
            if (nextCoreItem.evalConstraint(item.param, layout)) {
              val scope = ea.scopeOfTerminal(terminal)
              scopes.get(scope) match {
                case None => scopes += (scope -> (len, Set(t)))
                case Some((l, ts)) => 
                  if (len == l)
                    scopes += (scope -> (l, ts + t))
                  else if (len > l)
                    scopes += (scope -> (len, Set(t)))
              }
            }            
        }
      }
      item = item.nextItem
    }

    // create new items
    for ((scope, (len, _recognizedTerminals)) <- scopes) {
      val recognizedTerminals = ea.prioritizeTerminalsWithParams(_recognizedTerminals)
      val span = Span(column0, row, column, k, len)
      var item = bins(k).processedItems
      var destBin = bins(k + len)
      if (destBin == null) {
        destBin = new Bin(pool)
        bins(k + len) = destBin
      }
      while (item != null) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nextSymbol < 0) {
          val param = coreItem.evalParam(item.param, item.layout, coreItem.dot)
          val t = (coreItem.nextSymbol -> param)
          if (recognizedTerminals.contains(t)) {
            val layout = Span.addToLayout(item.origin, coreItem.includes, item.layout, span)
            val nextCoreItem = ea.coreItems(coreItem.nextCoreItem)
            if (nextCoreItem.evalConstraint(item.param, layout)) {
              destBin.addItem(coreItem.nextCoreItem, item.param, item.origin, layout)
            }
          }
        }
        item = item.nextItem
      }
    } 
  }

  def recognizedNonterminals(bin : Bin) : Set[Int] = {
    if (bin == null) return Set()
    var recognized : Set[Int] = Set()
    var item = bin.processedItems
    while (item != null) {
      if (item.origin == 0 && item.param == DEFAULT_PARAM) {
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
    for (k <- 0 until document.size) {
      scan(document, bins, k, predictAndComplete(bins, k))
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

  private class ParseTreeConstruction(document : Document, bins : Array[Bin]) {

    import scala.collection.mutable.{Map => MutableMap}
    private var cache : MutableMap[(Int, Int, Int, Int), ParseTree] = MutableMap()

    def getParseTree(nonterminal : Int, param : Int, startPosition : Int, endPosition : Int) : ParseTree = {
      val key = (nonterminal, param, startPosition, endPosition)
      cache.get(key) match {
        case None => 
          val result = constructParseTree(nonterminal, param, startPosition, endPosition)
          cache += (key -> result)
          result
        case Some(result) =>
          result
      }
    }

    /** Constructs the parse tree using the information obtained from the recognition phase. This assumes that there actually exists at least one parse tree.
      * @param startPosition the start position (inclusive)
      * @param endPosition the end position (exclusive)
      */
    def constructParseTree(nonterminal : Int, param : Int, startPosition : Int, endPosition : Int) : ParseTree = {
      val grammar = ea.grammar
      val ambiguityResolution = grammar.ambiguityResolution
      val nonterminalSymbol = ea.nonterminalOfId(nonterminal)
      val bin = bins(endPosition)
      var item = bin.processedItems
      var foundItems : List[Item] = List()
      while (item != null) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nonterminal == nonterminal && coreItem.nextSymbol == 0 && 
          item.param == param && item.origin == startPosition) 
        {
          foundItems = item :: foundItems
        }
        item = item.nextItem
      }
      def mkTree(foundItem : Item) : NonterminalNode = {
        val coreItem = ea.coreItemOf(foundItem)
        var subtrees = new Array[ParseTree](coreItem.rhs.size)
        var hasAmbiguities = false
        for (i <- 0 until subtrees.size) {
          val symbol = coreItem.rhs(i)
          val span = foundItem.layout(i)
          if (symbol < 0)
            subtrees(i) = TerminalNode(ea.terminalOfId(symbol), span)
          else {
            val p = coreItem.evalParam(param, foundItem.layout, i)
            subtrees(i) = getParseTree(symbol, p, span.firstIndexIncl, span.lastIndexExcl)
          }
          hasAmbiguities = hasAmbiguities || subtrees(i).hasAmbiguities
        }
        val ruleindex = coreItem.ruleindex
        val parserule = grammar.parserules(nonterminalSymbol)(ruleindex)
        val rhsIndices = grammar.rhsIndices(nonterminalSymbol, ruleindex)
        val span_ = Span.getSpanOfLayout(foundItem.layout)
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
        val value = 
          if (hasAmbiguities && !ambiguityResolution.isDefined) 
            null 
          else 
            parserule.action(new Context())
        NonterminalNode(nonterminalSymbol, ruleindex, span_, subtrees.toVector, value)
      }
      foundItems match {
        case List() => throw new RuntimeException("cannot construct parse tree")
        case List(foundItem) => mkTree(foundItem)
        case _ =>
          // to fix: if multiple spans are found here, not all of them might work for the constraint
          val trees = foundItems.map(mkTree _).toVector
          val node = trees.head
          if (ambiguityResolution.isDefined) {
            val v = ambiguityResolution.get.computeValue(node.nonterminal, node.span, trees)
            AmbiguousNode(node.nonterminal, node.span, trees, v)
          } else 
            AmbiguousNode(node.nonterminal, node.span, trees, null)
      }
    }

  }

  def parse(document : Document, nonterminalSymbol : String) : Either[ParseTree, Int] = {
    val nonterminal = ea.idOfNonterminal(nonterminalSymbol)
    recognize(document, Set(nonterminal)) match {
      case Left((recognized, bins)) =>
        val c = new ParseTreeConstruction(document, bins)
        Left(c.getParseTree(nonterminal, DEFAULT_PARAM, 0, document.size))
      case Right(k) => 
        Right(k) 
    }
  } 

}