package proofpeer.indent.earley

import proofpeer.indent._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}


object Earley {

  var debug : Boolean = false

  final val DEFAULT_PARAM : ParseParam.V = ParseParam.NIL
  final val DEFAULT_RESULT : ParseParam.V = ParseParam.NIL

  final class Item(val coreItemId : Int, val param : ParseParam.V, val origin : Int, 
    val layout : Span.Layout, val results : ParseParam.Results, 
    val nextSibling : Item, var nextItem : Item) 
  {
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

  var nonterminalItems : Item = null

  var terminalItems : Item = null

  var completedItems : Item = null

  private var newItems : Item = null 

  private var bitmap = pool.allocate()

  private var hadItems_ : Boolean = false

  def hadItems : Boolean = hadItems_

  def countItems(item : Item) : Int = {
    var num = 0
    var x = item
    while (x != null) {
      num += 1
      x = x.nextItem
    }
    num
  }

  def size : Int = 
    countItems(newItems) + 
    countItems(nonterminalItems) + 
    countItems(terminalItems) + 
    countItems(completedItems)

  def kindSizes(ea : EarleyAutomaton) : (Int, Int, Int) = {
    (countItems(terminalItems), countItems(nonterminalItems), countItems(completedItems))
  }

  def nextItem(ea : EarleyAutomaton) : Item = {
    if (newItems != null) {
      val item = newItems
      newItems = item.nextItem
      val nextSymbol = ea.coreItemOf(item).nextSymbol
      if (nextSymbol == 0) {
        item.nextItem = completedItems
        completedItems = item
      } else if (nextSymbol > 0) {
        item.nextItem = nonterminalItems
        nonterminalItems = item
      } else {
        item.nextItem = terminalItems
        terminalItems = item
      }
      item
    } else null
  }

  // return true if the item is new
  def addItem(coreItemId : Int, param : ParseParam.V, origin : Int, 
    layout : Span.Layout, results : ParseParam.Results) : Boolean = 
  {
    hadItems_ = true
    var item = bitmap(coreItemId)
    if (item == null) {
      newItems = new Item(coreItemId, param, origin, layout, results, null, newItems)
      bitmap(coreItemId) = newItems
      true
    } else if (item.param != param || item.origin != origin || 
      !Span.layoutsAreEqual(item.layout, layout) || 
      !ParseParam.resultsAreEqual(item.results, results))
    {
      var sibling = item.nextSibling
      while (sibling != null && (param != sibling.param || origin != sibling.origin 
        || !Span.layoutsAreEqual(layout, sibling.layout)
        || !ParseParam.resultsAreEqual(results, sibling.results)))
        sibling = sibling.nextSibling
      if (sibling == null) {
        newItems = new Item(coreItemId, param, origin, layout, results, item, newItems)
        bitmap(coreItemId) = newItems
        true
      } else false
    } else false
  }

  // take all the completed items and make them new again
  def renewCompleted() {
    while (completedItems != null) {
      val next = completedItems.nextItem
      completedItems.nextItem = newItems
      newItems = completedItems
      completedItems = next
    }
  }

  /** This is called when no new calls to [[addItem]] will be made. */
  def finishedAdding() {
    pool.release(bitmap)
    bitmap = null
    terminalItems = null
  }

  def release() {
    if (bitmap != null) finishedAdding()
  }

  def backup() : Bin.Backup = {
    var items = newItems
    var backup : Bin.Backup = Vector()
    while (items != null) {
      val backupItem : Bin.BackupItem = 
        (items.coreItemId, items.param, items.origin, items.layout, items.results)
      backup = backup :+ backupItem
      items = items.nextItem
    }
    backup
  }

}

object Bin {

  type BackupItem = (Int, ParseParam.V, Int, Span.Layout, ParseParam.Results)
  type Backup = Vector[BackupItem]

  def restore(pool : BitmapPool, backup : Backup) : Bin = {
    val bin = new Bin(pool)
    for (x <- backup) bin.addItem(x._1, x._2, x._3, x._4, x._5)
    bin
  }

}

trait Bins {
  def addBin(position : Int, bin : Bin)
  def apply(position : Int) : Bin
}

final class ArrayBins(size : Int) extends Bins {
  private val bins : Array[Bin] = new Array[Bin](size)
  def apply(position : Int) : Bin = bins(position)
  def addBin(position : Int, bin : Bin) { bins(position) = bin }
  def toArray : Array[Bin] = bins
}

final class Earley(ea : EarleyAutomaton) {

  import Earley._

  val pool = new BitmapPool(ea.coreItems.size)
  
  def initialBin(nonterminals : Set[Int], character : Int) : Bin = {
    val bin = new Bin(pool)
    for (coreItemId <- 0 until ea.coreItems.size) {
      val coreItem = ea.coreItems(coreItemId)
      if (coreItem.dot == 0 && nonterminals.contains(coreItem.nonterminal) && coreItem.first(character)) {
        val param = DEFAULT_PARAM
        val layout = Span.emptyLayout(0, coreItem.rhs.size)
        val results = ParseParam.emptyResults(param, layout, coreItem)
        if (coreItem.evalConstraint(param, layout, results))
          bin.addItem(coreItemId, param, 0, layout, results) 
      }
    }
    bin
  }

  def predictAndComplete(bins : Bins, k : Int, character : Int, terminals : MutableSet[(Int, ParseParam.V)]) : Boolean = {
    val bin = bins(k)
    if (bin == null) return false
    var item = bin.nextItem(ea)
    var repeat : Boolean = false
    while (item != null) {
      val coreItem = ea.coreItemOf(item)
      val param = item.param
      val nextSymbol = coreItem.nextSymbol
      val first = coreItem.first(character)
      if (nextSymbol < 0 && first) /* terminal */ {
        val nextSymbolParam = coreItem.evalParam(param, item.layout, item.results, coreItem.dot)
        terminals += (nextSymbol -> nextSymbolParam)
      } else if (nextSymbol > 0 && first) /* nonterminal */ {
        val nextSymbolParam = coreItem.evalParam(param, item.layout, item.results, coreItem.dot)
        for (predictedItem <- coreItem.predictedCoreItems) {
          val predictedCoreItem = ea.coreItems(predictedItem)
          if (predictedCoreItem.first(character)) {
            val layout = Span.emptyLayout(k, predictedCoreItem.rhs.size)
            val results = ParseParam.emptyResults(nextSymbolParam, layout, predictedCoreItem)
            if (predictedCoreItem.evalConstraint(nextSymbolParam, layout, results))
              repeat = bin.addItem(predictedItem, nextSymbolParam, k, layout, results) || repeat
          }
        }
        if (coreItem.nextSymbolIsNullable) {
          val nextCoreItemId = coreItem.nextCoreItem
          val nextCoreItem = ea.coreItems(nextCoreItemId)
          if (nextCoreItem.first(character)) {
            val layout = Span.addToLayout(item.origin, coreItem.rhs.size, item.layout, 
              Span.nullSpan(k, k))
            val results = ParseParam.addToResults(item.results, ea.nullresult(nextSymbol), param, layout, coreItem)
            if (nextCoreItem.evalConstraint(param, layout, results))
              repeat = bin.addItem(nextCoreItemId, param, item.origin, layout, results) || repeat
          }
        }
      } else if (nextSymbol == 0 /* && coreItem.dot > 0 */) /* no symbol, do completion for non-epsilon rules */ {
        val nonterminal = coreItem.nonterminal
        val span = Span.getSpanOfLayout(item.layout)
        val result = ParseParam.getResult(item.results)
        var originItem = bins(item.origin).nonterminalItems
        while (originItem != null) {
          val originCoreItem = ea.coreItemOf(originItem)
          if (originCoreItem.nextSymbol == nonterminal) {
            val p = originCoreItem.evalParam(originItem.param, originItem.layout, originItem.results, originCoreItem.dot)
            if (p == param) {
              val nextCoreItemId = originCoreItem.nextCoreItem
              val nextCoreItem = ea.coreItems(nextCoreItemId)
              if (nextCoreItem.first(character)) {
                val layout = Span.addToLayout(originItem.origin, originCoreItem.rhs.size, originItem.layout, span)
                val results = ParseParam.addToResults(originItem.results, result, originItem.param, layout, originCoreItem)
                if (nextCoreItem.evalConstraint(originItem.param, layout, results)) 
                  repeat = bin.addItem(nextCoreItemId, originItem.param, originItem.origin, layout, results) || repeat
              }
            }
          }
          originItem = originItem.nextItem
        }
      } 
      item = bin.nextItem(ea)
    }    
    repeat
  }

  private def isFallback(terminal : Int) : Boolean = {
    ea.scopeOfTerminal(terminal) == ea.fallbackScope
  }

  private def writeln(s : Any) {
    if (Earley.debug) println(s)
  }

  // returns repeat; repeat is true if the prediction/completion/scan process must be repeated
  def scan(document : Document, bins : Bins, k : Int, terminals : MutableSet[(Int, ParseParam.V)]) : Boolean =
  {
    if (terminals == null || terminals.isEmpty) return false

    def computeScopes(terminals : MutableSet[(Int, ParseParam.V)], fallback : Boolean) : 
      MutableMap[Int, (Int, Set[(Int, ParseParam.V, ParseParam.V)])] =
    {

      // check which terminals actually can be scanned from position k on
      val scans : MutableMap[(Int, ParseParam.V), (Int, ParseParam.V)] = MutableMap()
      for (t <- terminals) {
        if ((ea.scopeOfTerminal(t._1) == ea.fallbackScope) == fallback) {
          val lexer = ea.lexerOfTerminal(t._1)
          val (len, r) = lexer.lex(document, k, t._2)
          if (len > 0 || (len == 0 && isFallback(t._1))) {
            scans += (t -> (len, r))
          }
        }
      }

      // check which scans are compatible with some layout 
      val scopes : MutableMap[Int, (Int, Set[(Int, ParseParam.V, ParseParam.V)])] = MutableMap()
      var item = bins(k).terminalItems
      while (item != null) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nextSymbol < 0) {
          val terminal = coreItem.nextSymbol
          val terminalParam = coreItem.evalParam(item.param, item.layout, item.results, coreItem.dot)
          val t = (terminal -> terminalParam)
          scans.get(t) match {
            case None =>
            case Some((len, result)) =>
              val span = document.span(k, len) 
              val layout = Span.addToLayout(item.origin, coreItem.rhs.size, item.layout, span)
              val results = ParseParam.addToResults(item.results, result, item.param, layout, coreItem)
              val nextCoreItem = ea.coreItems(coreItem.nextCoreItem)
              if (nextCoreItem.evalConstraint(item.param, layout, results)) {
                val scope = ea.scopeOfTerminal(terminal)
                val tr = (t._1, t._2, result)
                scopes.get(scope) match {
                  case None => scopes += (scope -> (len, Set(tr)))
                  case Some((l, ts)) => 
                    if (len == l)
                      scopes += (scope -> (l, ts + tr))
                    else if ((scope != ea.fallbackScope && len > l) ||
                      (scope == ea.fallbackScope && (l == 0 || (len != 0 && len < l))))
                      scopes += (scope -> (len, Set(tr)))
                }
              }            
          }
        }
        item = item.nextItem
      }

      scopes
    }

    val scopes = {
      val scopes = computeScopes(terminals, false)
      if (scopes.isEmpty) computeScopes(terminals, true) else scopes
    }

    var repeat : Boolean = false

    // create new items
    for ((scope, (len, _recognizedTerminals)) <- scopes) {
      val recognizedTerminals = ea.prioritizeTerminalsWithParams(_recognizedTerminals)
      val span = document.span(k, len) 
      var item = bins(k).terminalItems
      var destBin = bins(k + len)
      if (destBin == null) {
        destBin = new Bin(pool)
        bins.addBin(k + len, destBin)
      }
      while (item != null) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nextSymbol < 0) {
          val param = coreItem.evalParam(item.param, item.layout, item.results, coreItem.dot)
          val candidateTerminals = recognizedTerminals.filter(t => t._1 == coreItem.nextSymbol && t._2 == param)
          if (!candidateTerminals.isEmpty) {
            val layout = Span.addToLayout(item.origin, coreItem.rhs.size, item.layout, span)
            val nextCoreItem = ea.coreItems(coreItem.nextCoreItem)
            for (t <- candidateTerminals) {
              val results = ParseParam.addToResults(item.results, t._3, item.param, layout, coreItem)
              if (nextCoreItem.evalConstraint(item.param, layout, results)) {
                if (destBin.addItem(coreItem.nextCoreItem, item.param, item.origin, layout, results))
                  if (len == 0) repeat = true
              }
            }
          }
        }
        item = item.nextItem
      }
    } 

    if (!repeat) bins(k).finishedAdding()

    repeat
  }

  def recognizedNonterminals(bin : Bin) : Set[Int] = {
    if (bin == null) return Set()
    var recognized : Set[Int] = Set()
    var item = bin.completedItems
    while (item != null) {
      if (item.origin == 0 && item.param == DEFAULT_PARAM) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nextSymbol == 0) recognized += coreItem.nonterminal
      }
      item = item.nextItem
    }
    recognized
  }

  def recognize(document : Document, nonterminals : Set[Int]) : Either[(Set[Int], Array[Bin]), Int] = {
    val bins = new ArrayBins(document.size + 1)
    val character = if (document.size == 0) -1 else document.character(0)._3
    bins.addBin(0, initialBin(nonterminals, character))
    for (k <- 0 to document.size) {
      val character = if (k == document.size) -1 else document.character(k)._3
      val terminals : MutableSet[(Int, ParseParam.V)] = MutableSet()
      var firstLoop = true
      do {
        if (!firstLoop) {
          bins(k).renewCompleted()
          firstLoop = false
        }
        while (predictAndComplete(bins, k, character, terminals)) {
          bins(k).renewCompleted()
        }
      } while (scan(document, bins, k, terminals))
    }
    val recognized = recognizedNonterminals(bins(document.size)).intersect(nonterminals)
    if (recognized.isEmpty) {
      var k = document.size
      var foundNonemptyBin = false
      while (k >= 0 && !foundNonemptyBin) {
        if (bins(k) != null && bins(k).hadItems) 
          foundNonemptyBin = true
        else k -= 1
      }
      Right(k)
    } else {
      Left((recognized, bins.toArray))
    } 
  }

  private class ParseTreeConstruction(document : Document, bins : Array[Bin]) {

    import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
    private val cache : MutableMap[(Int, ParseParam.V, ParseParam.V, Int, Int), ParseTree] = MutableMap()
    private val visiting : MutableSet[(Int, ParseParam.V, ParseParam.V, Int, Int)] = MutableSet()

    def getParseTree(nonterminal : Int, param : ParseParam.V, result : ParseParam.V,
      startPosition : Int, endPosition : Int) : ParseTree = 
    {
      val key = (nonterminal, param, result, startPosition, endPosition)
      cache.get(key) match {
        case None => 
          if (visiting(key)) return null
          visiting += key
          val r = constructParseTree(nonterminal, param, result, startPosition, endPosition)
          cache += (key -> r)
          visiting -= key
          r
        case Some(r) =>
          r
      }
    }

    /** Constructs the parse tree using the information obtained from the recognition phase. This assumes that there actually exists at least one parse tree.
      * @param startPosition the start position (inclusive)
      * @param endPosition the end position (exclusive)
      */
    def constructParseTree(nonterminal : Int, param : ParseParam.V, result : ParseParam.V, 
      startPosition : Int, endPosition : Int) : ParseTree = 
    {
      val grammar = ea.grammar
      val ambiguityResolution = grammar.ambiguityResolution
      val nonterminalSymbol = ea.nonterminalOfId(nonterminal)
      val bin = bins(endPosition)
      var item = bin.completedItems
      var foundItems : List[Item] = List()
      while (item != null) {
        val coreItem = ea.coreItemOf(item)
        if (coreItem.nonterminal == nonterminal && coreItem.nextSymbol == 0 && 
          item.param == param && item.origin == startPosition) 
        {
          if (result != null) {
            val r = ParseParam.getResult(item.results)
            if (r == result) foundItems = item :: foundItems
          } else 
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
            val p = coreItem.evalParam(param, foundItem.layout, foundItem.results, i)
            val r = foundItem.results(i)
            subtrees(i) = getParseTree(symbol, p, r, span.firstIndexIncl, span.lastIndexExcl)
            if (subtrees(i) == null) return null
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
        case List() => throw new RuntimeException("cannot construct parse tree for " + 
          ea.nonterminalOfId(nonterminal) + " from " + startPosition + " to " + endPosition)
        case List(foundItem) => mkTree(foundItem)
        case _ =>
          val trees = foundItems.map(mkTree _).toVector.filter(t => t != null)
          val node = trees.head
          if (trees.size == 1) node
          else if (ambiguityResolution.isDefined) {
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
        Left(c.getParseTree(nonterminal, DEFAULT_PARAM, null, 0, document.size))
      case Right(k) => 
        Right(k) 
    }
  } 

}

