package proofpeer.indent.lr1

import proofpeer.indent._

final class LR1Parser(grammar : Grammar, startNonterminal : String, printErrors : Boolean = true) {
  if (!grammar.isWellformed) {
    if (printErrors) {
      val errors = grammar.errors
      println("The grammar given to the parser has " + errors.size + " errors:")
      for (i <- 1 to errors.size) {
        println("  " + i + ". " + errors(i - 1))
      }
      println("")
    }
    throw new RuntimeException("grammar is not wellformed, cannot create parser")  
  }

  private val automaton = new LR1Automaton(grammar, startNonterminal)

  if (!automaton.consistent) throw new RuntimeException("grammar is not LR(1)")
  
  def lr1 : LR1 = new LR1(automaton)
  
  def parse[T](text : String) : Option[T] = {
    lr1.parse(Document.fromString(text)) match {
      case Left(parsetree) => Some(parsetree.getValue[T])
      case Right(errorposition) => None
    }      
  }

  def parseAsTree(text : String) : Option[(Document, ParseTree)] = {
    val d = Document.fromString(text)
    lr1.parse(d) match {
      case Left(parsetree) => Some((d, parsetree))
      case Right(errorposition) => None
    }            
  }

}


final class LR1(lr1 : LR1StateMachine) {

  // returns a pair (terminal, len) if successful
  def scan(document : Document, k : Int, terminals : Set[Int]) : Option[(Int, Int)] =
  {
    if (terminals == null || terminals.isEmpty) return None
    if (terminals.contains(0) && k == document.size) return Some((0, 0))

    def error[T](t1 : Int, t2 : Int) : T = {
      val s1 = lr1.symbolOfId(t1)
      val s2 = lr1.symbolOfId(t2)
      throw new RuntimeException("scan conflict at position " + k + " between terminals " + s1 + " and " + s2)
    }

    def compute(fallback : Boolean) : Option[(Int, Int)] = {
      // scope -> (terminal, len, prio)
      var scans : Map[String, (Int, Int, Option[Int])] = Map()
      for (t <- terminals - 0) {
        val rule = lr1.grammar.scanrules(lr1.symbolOfId(t)) 
        if ((rule.scope == FALLBACK_SCOPE) == fallback) {
          val (len, _) = rule.lexer.lex(document, k, ParseParam.NIL)
          if (len >= 0) {
            var update = false
            scans.get(rule.scope) match {
              case None =>
                update = true
              case Some((t0, len0, prio0)) =>
                if (len0 == len) {
                  (prio0, rule.priority) match {
                    case (Some(p0), Some(p)) => 
                      if (p0 == p) error(t0, t)
                      else if (p0 < p) update = true
                    case _ =>
                      error(t0, t)
                  }
                } else if (len0 == 0) {
                  update = true
                } else if (len == 0) {
                  update = false
                } else if (fallback) {
                  update = (len < len0)
                } else {
                  update = (len > len0)
                }
            }
            if (update) scans += (rule.scope -> (t, len, rule.priority))
          }
        }
      }
      scans.size match {
        case 0 => None
        case 1 => 
          val r = scans.head._2
          Some(r._1 -> r._2)
        case _ => 
          val r1 = scans.head._2
          val r2 = scans.tail.head._2
          error(r1._1, r2._1)
      }
    }

    compute(false) match {
      case None => compute(true)
      case result => result
    }
  }

  private def isFinalStack(valueStack : List[(Int, ParseTree)]) : Boolean = {
    valueStack match {
      case List((t, _)) => t == lr1.startNonterminalId
      case _ => false
    }
  }

  private def computeSpan(k : Int, trees : Vector[ParseTree]) : Span = {
    var span = Span.nullSpan(k)
    for (tree <- trees) span.addBehind(tree.span)
    span
  }

  def parse(doc : Document) : Either[ParseTree, Int] = {
    var valueStack : List[(Int, ParseTree)] = List()
    var k : Int = 0
    var stateStack : List[(Int, Int)] = List(k -> lr1.startState)
    var X : Option[(Int, Int)] = None
    def done : Boolean = {
      stateStack.head._2 == lr1.startState && k == doc.size && isFinalStack(valueStack)  
    }
    do {
      val state = stateStack.head._2
      if (done) return Left(valueStack.head._2)
      if (X == None) {
        X = scan(doc, k, lr1.lookahead(state))
        if (X == None) return Right(k)
      }
      val x = X.get
      lr1.shift(state, x._1) match {
        case Some(state) =>
          val tree = TerminalNode(lr1.symbolOfId(x._1), doc.span(k, x._2))
          valueStack = (x._1, tree) :: valueStack
          k += x._2
          stateStack = (k -> state) :: stateStack
          X = None
        case None =>
          lr1.reduce(state, x._1) match {
            case Some(reduction) =>
              val coreItem = lr1.coreItemAt(reduction)
              val nonterminal = lr1.symbolOfId(coreItem.nonterminal)
              val n = coreItem.rhs.size
              val parserule = lr1.grammar.parserules(nonterminal)(coreItem.ruleindex)
              val trees = valueStack.take(n).map(_._2).toVector.reverse
              valueStack = valueStack.drop(n)
              stateStack = stateStack.drop(n)
              val _span = computeSpan(stateStack.head._1, trees)
              val context = new ParseContext {
                def result(indexedSymbol : IndexedSymbol) : ParseTree = {
                  val index = parserule.rhs.indexOf(indexedSymbol)
                  if (index < 0) throw new RuntimeException("no symbol " + indexedSymbol + " found on right hand side of rule")
                  trees(index)
                }
                def document : Document = doc
                def grammar : Grammar = lr1.grammar
                def rule : ParseRule = parserule
                def span : Span = _span
                def startPosition : Int = _span.firstIndexIncl
                def endPosition : Int = _span.lastIndexExcl               
              }
              val v = parserule.action(context)
              val tree = NonterminalNode(nonterminal, coreItem.ruleindex, _span, trees, v)
              valueStack = (coreItem.nonterminal -> tree) :: valueStack
              lr1.shift(stateStack.head._2, coreItem.nonterminal) match {
                case Some(state) =>
                  stateStack = (k -> state) :: stateStack
                case None =>
                  if (done) return Left(valueStack.head._2)
                  else return Right(k)
              }
            case None =>
              throw new RuntimeException("internal error: neither shift nor reduce action found at position " + k + " for terminal " + lr1.symbolOfId(x._1))
          }
      }
    } while (true)
    throw new RuntimeException("internal error")
  } 

}

