package proofpeer.indent.earley

import proofpeer.indent.API
import proofpeer.indent.API.Nonterminal
import proofpeer.indent.API.LexicalPriority
import proofpeer.indent.Derivation
import proofpeer.indent.Derivation.ValueNonterminal
import proofpeer.indent.API.Grammar
import proofpeer.indent.Document
import proofpeer.indent.UnicodeDocument
import proofpeer.indent.Span
import proofpeer.indent.Token
import proofpeer.indent.EvalConstraints
import scala.collection.immutable._


trait Recognizer {
  def grammar : Grammar
  def parse(document : Document, start : Nonterminal, k : Int) : Option[(Recognizer.Value, Int)] 
  def parse(document : String, start : Nonterminal) : Option[Recognizer.Value] = {
    val d = UnicodeDocument.fromString(document)
    parse(d, start, 0) match {
      case None => None
      case Some((v, i)) => if (i == d.size) Some(v) else None
    }
  }
}


object Recognizer {

  var blackboxCalls : Int = 0
  var maxduration : Long = 0
  var totalduration : Long = 0

  var totalFinalvalues = 0
  var ambiguousFinalvalues = 0

  case class Value(span : Option[Span], unique : Boolean)
  type IValue = Vector[Value]

  case class R(ruleindex : Int, rule: API.Rule[Int]) extends Rule {
    def rhsSize = rule.rhs.size
    def rhsAt(i : Int) = rule.rhs(i).indexedSymbol.symbol 
    def check_constraint(values : Vector[Value]) : Boolean = {
      def spans(i : Int) : Option[Span] = 
        if (i >= values.size) None else values(i).span
      EvalConstraints.eval(rule.constraint, spans : EvalConstraints.Spans[Int]) != EvalConstraints.False
    }
    def finalValue(i : Int, j : Int, values : Vector[Value]) : Value = {
      var span : Option[Span] = None
      var unique = true
      for (k <- 0 to (values.size - 1)) {
        val v = values(k)
        unique = unique && v.unique
        (span, v.span) match {
          case (None, s) => span = s
          case (s, None) => span = s
          case (Some(u), Some(v)) => span = Some(u.addBehind(v))
        }
      }
      Value(span, unique)
    }
  } 
  
  type Rules = Map[Nonterminal, Vector[R]]
  
  def adaptRules(grammar : API.Grammar) : (Rules, Rules) = {
    def addRule(rules : Rules, r : R) : Rules = {
      val nonterminal = r.rule.lhs
      rules.get(nonterminal) match {
        case None => rules + (nonterminal -> Vector(r))
        case Some(v) => rules + (nonterminal -> (v :+ r))
      }
    }
    val lexicals = grammar.info.lexicals
    var ruleindex : Int = 0
    var outerRules : Rules = Map()
    var innerRules : Rules = Map()
    for (rule <- grammar.info.rules) {
      val r = R(ruleindex, rule)
      ruleindex += 1
      if (lexicals.contains(rule.lhs)) {
        innerRules = addRule(innerRules, r)
        outerRules += (rule.lhs -> Vector())
      } else
        outerRules = addRule(outerRules, r)
    }
    (outerRules, innerRules)
  }
  
    
  class D(val size : Int, document : Document) extends Document {
    def getToken(position : Int) = document.getToken(position)
    def getText(position : Int, len : Int) = document.getText(position, len)
  }
  
  private val iValue : IValue = Vector()
  
  abstract class G(val rules : Rules, val debug : Boolean) extends BlackboxGrammar[Value, IValue] {
        
    def rulesOfNonterminal(nonterminal : Nonterminal) = rules(nonterminal)
    
    def valueOfToken(token : Token) : Value = Value(Some(token.span), true)
    
    def initialValue(document : Document, i : Int, nonterminal : Nonterminal, ruleindex : Int) = iValue
    
    def nextValue(document : Document, i : Int, j : Int, k : Int, nonterminal : Nonterminal, ruleindex : Int,
      dot : Int, intermediateValue : IValue, symbolValue : Value) : Option[IValue] = 
    {
      val r = rules(nonterminal)(ruleindex)
      val vnext = intermediateValue :+ symbolValue
      if (r.check_constraint(vnext)) Some(vnext) else None
    }
    
    def finalValue(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int,
      value : IValue) : Value =
    {
      val r = rules(nonterminal)(ruleindex)
      r.finalValue(i, j, value)
    }
    
    def mergeValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int, dot : Int,
      value1 : IValue, value2 : IValue) : Option[IValue] =
    {
      None
      /*val value = value1 ++ value2
      if (value.size == value1.size) None else Some(value)*/
    }
    
    def joinValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, 
      value1 : Value, value2 : Value) : Value =
    {
      Value(value1.span, false)
    }  
    
  }
  
  class GInner(rules : Rules) extends G(rules, false) {

    def hasBlackboxes = false
    
    def isBlackbox(nonterminal : Nonterminal) = false
    
    def callBlackboxes(document : Document, i : Int, blackboxes : Set[Nonterminal]) :
      Map[Nonterminal, Seq[(Int, Value)]] = Map()
        
  }
  
  class GOuter(outerRules : Rules, innerRules : Rules, lexicals : Map[Nonterminal, LexicalPriority]) 
    extends G(outerRules, true) 
  {
    val gInner = new GInner(innerRules)
    
    def hasBlackboxes = !lexicals.isEmpty
    
    def isBlackbox(nonterminal : Nonterminal) = lexicals.contains(nonterminal)
    
    def callBlackboxes(document : Document, i : Int, blackboxes : Set[Nonterminal]) :
      Map[Nonterminal, Seq[(Int, Value)]] = 
    {
      val t1 = System.currentTimeMillis
      blackboxCalls += 1
      val d = document
      val earley = new Earley(gInner, d, false)
      val (bins, k) = earley.recognize(blackboxes, i)
      val (results, l) = earley.compute_longest_parse_values(blackboxes.contains(_), bins, i, k)
      val result : Map[Nonterminal, Seq[(Int, Value)]] = if (results.isEmpty) 
        Map()
      else {
        var prio : Map[Int, (Int, Map[Nonterminal, Value])] = Map()
        for ((n, v) <- results) {
          val p = lexicals(n)
          val newPriority = p.priority
          prio.get(p.scope) match {
            case None =>
              prio = prio + (p.scope -> (newPriority, Map(n -> v)))
            case Some((currentPriority, m)) =>
              if (newPriority == currentPriority) {
                val newM = m + (n -> v)
                prio = prio + (p.scope -> (currentPriority, newM))
              } else if (newPriority > currentPriority) {
                val newM = Map(n -> v)
                prio = prio + (p.scope -> (newPriority, newM))
              }
          }
        }
        def add(m : Map[Nonterminal, Seq[(Int, Value)]], additions : Map[Nonterminal, Value]) : Map[Nonterminal, Seq[(Int, Value)]] = {
          var newM = m
          for ((n, v) <- additions) newM = newM + (n -> Seq((l, v)))
          newM
        }
        var m : Map[Nonterminal, Seq[(Int, Value)]] = Map()
        for ((_, (_, additions)) <- prio) m = add(m, additions)
        m
      }
      val t2 = System.currentTimeMillis
      val duration = t2 - t1
      if (duration > maxduration) maxduration = duration
      totalduration = totalduration + duration
      result
    }

    
  }
  

  def recognizer(apigrammar : API.Grammar) : Recognizer = {
    if (!apigrammar.wellformed) throw new RuntimeException("Cannot parse with illformed grammar.")
    val (outerRules, innerRules) = adaptRules(apigrammar)
    val g = new GOuter(outerRules, innerRules, apigrammar.info.lexicals)
    new Recognizer {
      def grammar = apigrammar
      def parse(document : Document, start : Nonterminal, k : Int) : 
        Option[(Value, Int)] =
      {
        val earley = new Earley(g, document, true)
        earley.parse(start, k)       
      }
    }
  } 

}