package proofpeer.indent.earley

import proofpeer.indent.API
import proofpeer.indent.API.Nonterminal
import proofpeer.indent.API.LexicalPriority
import proofpeer.indent.Derivation
import proofpeer.indent.Derivation.ValueNonterminal
import proofpeer.indent.Document
import proofpeer.indent.Span
import proofpeer.indent.Token
import proofpeer.indent.EvalConstraints
import scala.collection.immutable._


object Adapter {
  
  case class R(ruleindex : Int, rule: API.Rule[Int]) extends Rule {
    def rhsSize = rule.rhs.size
    def rhsAt(i : Int) = rule.rhs(i).indexedSymbol.symbol 
    def ignore_layout(i : Int) = rule.rhs(i).ignore_layout
    def check_constraint(values : Vector[Value]) : Boolean = {
      def spans(i : Int) : Option[Span] = {
        if (i < values.size) 
          Derivation.spanOfValue(values(i))
        else 
          None
      }
      EvalConstraints.eval(rule.constraint, spans : EvalConstraints.Spans[Int]) != EvalConstraints.False
    }
    def finalValues(i : Int, j : Int, values : Vector[Value]) : Seq[ValueNonterminal] = {
      var span : Option[Span] = None
      for (k <- 0 to (values.size - 1)) {
        if (!ignore_layout(k)) {
          (span, Derivation.spanOfValue(values(k))) match {
            case (None, s) => span = s
            case (s, None) => span = s
            case (Some(u), Some(v)) => span = Some(u.addBehind(v))
          }
        }
      }
      var seq : Seq[ValueNonterminal] = Seq()
      val tree = Derivation.Tree(ruleindex, values)
      val (treeOpt, cycle_value) = Derivation.prevent_cycle(rule.lhs, i, j, tree)
      if (treeOpt.isDefined) 
        seq :+= ValueNonterminal(i, j, span, rule.lhs, Set(treeOpt.get))
      if (cycle_value.isDefined) {
        throw new RuntimeException("BOOM!!")
        seq :+= cycle_value.get
      }
      seq
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
  
  type Value = Derivation.Value
  type IValue = Set[Vector[Value]]
    
  class D(val size : Int, document : Document) extends Document {
    def getToken(position : Int) = document.getToken(position)
    def getText(position : Int, len : Int) = document.getText(position, len)
  }
  
  private def iValue : IValue = Set(Vector())
  
  abstract class G(rules : Rules) extends BlackboxGrammar[Value, IValue] {
        
    def rulesOfNonterminal(nonterminal : Nonterminal) = rules(nonterminal)
    
    def valueOfToken(token : Token) : Value = { Derivation.ValueToken(token) }
    
    def initialValue(document : Document, i : Int, nonterminal : Nonterminal, ruleindex : Int) = iValue
    
    def nextValue(document : Document, i : Int, j : Int, k : Int, nonterminal : Nonterminal, ruleindex : Int,
      dot : Int, intermediateValue : IValue, symbolValue : Value) : Option[IValue] = 
    {
      val r = rules(nonterminal)(ruleindex)
      val vnext = intermediateValue.flatMap { oldV =>
        val newV = oldV :+ symbolValue
        if (r.check_constraint(newV)) Some(newV) else None
      }
      if (vnext.isEmpty) None else Some(vnext)
    }
    
    def finalValue(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int,
      value : IValue) : Value =
    {
      val r = rules(nonterminal)(ruleindex)
      Derivation.join(value.flatMap(r.finalValues(i, j, _)).to[Seq]).get
    }
    
    def mergeValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int, dot : Int,
      value1 : IValue, value2 : IValue) : Option[IValue] =
    {
      val value = value1 ++ value2
      if (value.size == value1.size) None else Some(value)
    }
    
    def joinValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, 
      value1 : Value, value2 : Value) : Value =
    {
      val v1 = value1.asInstanceOf[ValueNonterminal]
      val v2 = value2.asInstanceOf[ValueNonterminal]
      Derivation.join(Seq(v1, v2)).get
    }  
    
  }
  
  class GInner(rules : Rules) extends G(rules) {

    def hasBlackboxes = false
    
    def isBlackbox(nonterminal : Nonterminal) = false
    
    def callBlackboxes(document : Document, i : Int, blackboxes : Set[Nonterminal]) :
      Map[Nonterminal, Seq[(Int, Value)]] = Map()
        
  }
  
  class GOuter(outerRules : Rules, innerRules : Rules, lexicals : Map[Nonterminal, LexicalPriority]) 
    extends G(outerRules) 
  {
    val gInner = new GInner(innerRules)
    
    def hasBlackboxes = !lexicals.isEmpty
    
    def isBlackbox(nonterminal : Nonterminal) = lexicals.contains(nonterminal)
    
    def callBlackboxes(document : Document, i : Int, blackboxes : Set[Nonterminal]) :
      Map[Nonterminal, Seq[(Int, Value)]] = 
    {
      val d = document
      val earley = new Earley(gInner, d)
      val (bins, k) = earley.recognize(blackboxes, i)
      val (results, l) = earley.compute_longest_parse_values(blackboxes.contains(_), bins, i, k)
      if (results.isEmpty) 
        Map()
      else {
        var nonprio : Map[Nonterminal, Value] = Map()
        var prio : Map[Int, (Int, Map[Nonterminal, Value])] = Map()
        for ((n, v) <- results) {
          val p = lexicals(n)
          if (!p.priority.isDefined) 
            nonprio = nonprio + (n -> v)
          else {
            val newPriority = p.priority.get
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
        }
        def add(m : Map[Nonterminal, Seq[(Int, Value)]], additions : Map[Nonterminal, Value]) : Map[Nonterminal, Seq[(Int, Value)]] = {
          var newM = m
          for ((n, v) <- additions) newM = newM + (n -> Seq((l, v)))
          newM
        }
        var m = add(Map(), nonprio)
        for ((_, (_, additions)) <- prio) m = add(m, additions)
        m
      }
    }
    
  }
  
  def parser(apigrammar : API.Grammar) : API.Parser = {
    if (!apigrammar.wellformed) throw new RuntimeException("Cannot parse with illformed grammar.")
    val (outerRules, innerRules) = adaptRules(apigrammar)
    val g = new GOuter(outerRules, innerRules, apigrammar.info.lexicals)
    new API.Parser {
      def grammar = apigrammar
      def parse(document : Document, start : Nonterminal, k : Int) : 
        Option[(Derivation.Value, Int)] =
      {
        val earley = new Earley(g, document)
        earley.parse(start, k)       
      }
    }
  } 

}