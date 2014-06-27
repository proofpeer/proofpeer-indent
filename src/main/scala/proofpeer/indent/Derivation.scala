package proofpeer.indent

import API._
import scala.collection.immutable._
import scala.language.dynamics

object Derivation {

  sealed abstract class Value {
    def isUnique : Boolean;
    def wrap(grammar : Grammar, document : Document) : ValueWrapper = wrap(grammar, document, false)
    def wrap(grammar : Grammar, document : Document, pickAnyDerivation : Boolean) : ValueWrapper = {
      new ValueWrapper(grammar, document, this, pickAnyDerivation)
    }
    def span : Option[Span]
  }
  
  case class ValueToken(token : Token) extends Value {
    def isUnique : Boolean = true
    def span = Some(token.span)
  }
  
  case class ValueNonterminal(i : Int, j : Int, span : Option[Span], 
      nonterminal : Nonterminal, derivations : Set[Tree]) extends Value
  {
    def isUnique : Boolean = {
      if (derivations.size != 1) return false
      val tree = derivations.head
      tree.rhs.forall(_.isUnique)
    }
  }
  
  case class Tree(ruleindex : Int, rhs : Vector[Value])
  
  /** Returns true iff all derivations represented by value1 are also derivations represented by 
    * value2.
    */
  def leq(value1 : Value, value2 : Value) : Boolean = {
    (value1, value2) match {
      case (ValueToken(t1), ValueToken(t2)) => t1.i == t2.i
      case (ValueNonterminal(i1, j1, span1, nonterminal1, derivations1), 
            ValueNonterminal(i2, j2, span2, nonterminal2, derivations2)) =>   
        i1 == i2 && j1 == j2 && nonterminal1 == nonterminal2 &&
          derivations1.forall(d1 => derivations2.exists(d2 => leq(d1, d2)))
      case _ => false
    }
  }
  
  /** Returns true iff all derivations represented by tree1 are also derivations represented by 
    * tree2. Only trees that are stored in the same ValueNonterminal.derivations will be compared.
    */  
  def leq(tree1 : Tree, tree2 : Tree) : Boolean = {
    tree1.ruleindex == tree2.ruleindex && tree1.rhs.size == tree2.rhs.size &&
      (0 to (tree1.rhs.size - 1)).forall(i => leq(tree1.rhs(i), tree2.rhs(i)))
  }
  
  def norm(trees : Seq[Tree]) : Set[Tree] = {
    def op(col : Seq[Tree], t : Tree) : Seq[Tree] = {
      if (col.exists(c => leq(t, c)))
        col
      else
        col.filter(c => !leq(c, t)) :+ t
    }
    trees.foldLeft(Seq[Tree]())(op).toSet
  }
  
  def join(values : Seq[ValueNonterminal]) : Option[ValueNonterminal] = {
    if (values.isEmpty) 
      None
    else {
      val v = values.head
      val derivations = norm(values.flatMap(_.derivations))
      Some(ValueNonterminal(v.i, v.j, v.span, v.nonterminal, derivations))
    }
  }
  
  /** Returns a pair (u, v) where u is the part of value
    * that does not contain already a derivation for the given nonterminal
    * for the given region, and v is the actual derivation for the given nonterminal
    * for the given region. Either u or v can be None, but not both at the same time.
    */
  def prevent_cycle(nonterminal : Nonterminal, i : Int, j : Int, value : Value) : 
    (Option[Value], Option[ValueNonterminal]) =
  {
    value match {
      case value : ValueNonterminal if value.i == i && value.j == j =>
        if (value.nonterminal == nonterminal)
          (None, Some(value))
        else {
          val trees_and_values = value.derivations.map(prevent_cycle(nonterminal, i, j, _))
          val trees = norm(trees_and_values.flatMap(_._1).to[Seq])
          val cycle_value = join(trees_and_values.flatMap(_._2).to[Seq])
          if (trees.isEmpty)
            (None, cycle_value)
          else
            (Some(ValueNonterminal(i, j, value.span, value.nonterminal, trees)), cycle_value)
        }
      case _  => (Some(value), None)
    }
  }
  
  def prevent_cycle(nonterminal : Nonterminal, i : Int, j : Int, valuevec : Vector[Value]) :
    (Option[Vector[Value]], Option[ValueNonterminal]) =
  {
    val pvalues = valuevec.map(prevent_cycle(nonterminal, i, j, _))
    val cycle_value = join(pvalues.flatMap(_._2).to[Seq])
    val values = pvalues.flatMap(_._1)
    if (values.size < pvalues.size) 
      (None, cycle_value)
    else
      (Some(values), cycle_value)
  }
  
  def prevent_cycle(nonterminal : Nonterminal, i : Int, j : Int, tree : Tree) :
    (Option[Tree], Option[ValueNonterminal]) =
  {
    val (values, cycle_value) = prevent_cycle(nonterminal, i, j, tree.rhs)
    values match {
      case None => (None, cycle_value)
      case Some(values) => (Some(Tree(tree.ruleindex, values)), cycle_value)
    }
  }
  
  def spanOfValue(value : Value) : Option[Span] = {
    value match {
      case ValueToken(t) => Some(t.span)
      case value : ValueNonterminal => value.span
    }
  }
  
  def visualize(grammar : API.Grammar, prefix : String, value : Value,
      display : Value => Boolean) : Array[String] = 
  {
    if (!display(value)) return Array()
    value match {
      case ValueToken(token) => Array(prefix + "token: " + token.terminal)
      case ValueNonterminal(i, j, span, nonterminal, derivations) =>
        if (derivations.size == 1) 
          visualize(grammar, prefix, derivations.head, display)
        else {
          var output = Array(prefix + "==========================") 
          val ordering = Ordering.by[Array[String], String](_.fold("")(_ + _ + "/"))
          var sorted : SortedSet[Array[String]] = SortedSet()(ordering)
          for (tree <- derivations) 
            sorted += visualize(grammar, prefix, tree, display)         
          var first = true
          for (tree <- sorted) {
            if (first) 
              first = false
            else
              output = output :+ (prefix + "--------------------------")
            output = output ++ tree 
           }
           output = output :+ (prefix + "==========================")
           output
        }
    }
  }
  
  private def symbolOfValue(value : Value) : String = {
    value match {
      case ValueToken(t) => t.terminal.toString
      case v: ValueNonterminal => v.nonterminal.toString
    }
  }
  
  def visualize(grammar : API.Grammar, prefix : String, tree : Tree,
      display : Value => Boolean) : Array[String] = 
  {
    val rule = grammar.rules(tree.ruleindex)
    var output : Array[String] = Array()
    var rhs = (0 to tree.rhs.size - 1).map{ case i =>
      val v = tree.rhs(i)
      if (display(v)) 
        rule.rhs(i).toString 
      else
          symbolOfValue(tree.rhs(i))}.
      fold("")((x, y) => x + " " + y)
    //var rhs = rule.rhs.map(_.toString).fold("")((x, y) => x + " " + y)
    if (!rhs.isEmpty()) rhs = rhs.substring(1)
    output = output :+ (prefix + rule.lhs + " => " + rhs)
    for (value <- tree.rhs) output = output ++ 
      visualize(grammar, "  " + prefix, value, display)
    output
  }
  
  def visualize(grammar : API.Grammar, value : Value, display : Value => Boolean) 
    : String = 
  {
    visualize(grammar, "", value, display).fold("")((x, y) => x + "\n" + y)
  }
  
  def defaultDisplay(value : Value) : Boolean = 
    value match {
      case _ : ValueToken => false
      case v : ValueNonterminal => true
  }

  def visualize(grammar : API.Grammar, value : Value) 
    : String = 
  {
    visualize(grammar, "", value, defaultDisplay(_)).fold("")((x, y) => x + "\n" + y)
  }
  
  class ValueWrapper(grammar : API.Grammar, document : Document, value : Value, pickAnyDerivation : Boolean) extends Dynamic {
    
    def _value : Value = value
    
    def _text : String = {
      value.span match {
        case None => ""
        case Some(span) => document.getText(span.firstTokenIndex, span.lastTokenIndex - span.firstTokenIndex + 1)
      }
    }
    
    def _lookup(name : String) : ValueWrapper = _lookup(name, pickAnyDerivation)
    
    def _lookup(name : String, pickAnyDerivation : Boolean) : ValueWrapper = {
      value match {
        case _ : ValueToken => 
          throw new RuntimeException("Cannot lookup '"+name+"' within a token value.")
        case value : ValueNonterminal =>
          if (value.derivations.isEmpty) 
            throw new RuntimeException("Cannot lookup '"+name+
                "', there are no derivations for nonterminal '"+value.nonterminal+"'.")
          val tree = value.derivations.head // pick one of the derivations
          if (!pickAnyDerivation && value.derivations.size != 1)
            throw new RuntimeException("Cannot lookup '"+name+
                "', there are multiple derivations for nonterminal '"+value.nonterminal+"'.")
          val rule = grammar.rules(tree.ruleindex) 
          var i = 0
          for (symbol <- rule.rhs) {
            if (symbol.indexedSymbol.toString == name) {
              return new ValueWrapper(grammar, document, tree.rhs(i), pickAnyDerivation)
            }
            i = i + 1
          }
          return null
      }
    }
    
    def selectDynamic(name : String) : ValueWrapper = _lookup(name)
    
  }  
  
  sealed class ParseResult(val grammar : Grammar, val document : Document, val _result : Any, val value : Value) {
    def resultAs[T]() : T = _result.asInstanceOf[T]
    def result : Any = _result
    def span : Option[Span] = value.span
    def text : String = {
      span match {
        case None => ""
        case Some(span) => document.getText(span.firstTokenIndex, span.lastTokenIndex - span.firstTokenIndex + 1)
      }      
    } 
    def codes : Vector[Int] = proofpeer.scala.lang.codePoints(text)
  }
  
  sealed class Context(val _grammar : Grammar, val _document : Document, val _value : Value, 
                       val _input : Map[IndexedSymbol, ParseResult]) extends Dynamic 
  {
    import APIConversions._

    def _span : Option[Span] = _value.span
    
    def _text : String = {
      _span match {
        case None => ""
        case Some(span) => _document.getText(span.firstTokenIndex, span.lastTokenIndex - span.firstTokenIndex + 1)
      }
    }
       
    def selectDynamic(name : String) : ParseResult = {
      _input.get(name) match {
        case None =>
          throw new RuntimeException("no symbol '"+name+"' in rule found")
        case Some(r) => r
      }
    }
  } 
  
  def computeParseResult(grammar : Grammar, document : Document, 
      tokenResult : ValueToken => Any, value : Value) : ParseResult = 
  {
    value match {
      case v : ValueToken => 
        new ParseResult(grammar, document, tokenResult(v), value)
      case v : ValueNonterminal =>
        if (v.derivations.size != 1) 
          throw new RuntimeException("Cannot construct parse result for ambiguous value of: "+v.nonterminal)
        else {
          val tree = v.derivations.head
          val rule = grammar.rules(tree.ruleindex)
          var i = 0
          var m : Map[IndexedSymbol, ParseResult] = Map()
          for (s <- rule.rhs) {
            val result = computeParseResult(grammar, document, tokenResult, tree.rhs(i))
            m += (s.indexedSymbol -> result)
            i = i + 1
          }
          val result = rule.action(new Context(grammar, document, value, m))
          new ParseResult(grammar, document, result, value)
        }       
    }   
  }

}

