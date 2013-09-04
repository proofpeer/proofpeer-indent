package proofpeer.indent

import scala.collection.immutable._

/** Provides tools for checking if a grammar is wellformed. 
  *  
  * A grammar is wellformed iff:
  * 
  *  - All constraints are wellformed, i.e. constraints uniquely reference
  *    symbols on the right hand side of the rule.
  *  - Lexical nonterminals must not depend on normal nonterminals, only on lexical nonterminals.
  *  - Lexical nonterminals may not be nullable. 
  *  - Only lexical nonterminals can be prioritized.
  */
object GrammarChecker {
  
  import API._

  /** Denotes an error that makes the grammar illformed. */
  abstract class GrammarError {
    def rules : List[Int]
    def annotations : List[Int]
  }
  
  /** The constraint in the given rule references a symbol that does not 
    * appear on the right hand side of the rule.
    */
  case class UndeclaredSymbolInConstraint(ruleindex : Int, symbol : IndexedSymbol) extends GrammarError 
  { 
    def rules = List(ruleindex) 
    def annotations = List()
  }
  
  /** The constraint in the given rule references a symbol which appears multiple times
    * on the right hand side of the rule.
    */
  case class AmbiguousSymbolInConstraint(ruleindex : Int, symbol : IndexedSymbol) extends GrammarError 
  {
    def rules = List(ruleindex)
    def annotations = List()
  }
  
  /** A lexical nonterminal depends on a normal nonterminal, i.e. a nonterminal that is not lexical. */
  case class InvalidLexicalDependency(ruleindex : Int, annotationindex : Int,
      lexical : Nonterminal, normal : Nonterminal) extends GrammarError 
  {
    def rules = List(ruleindex)
    def annotations = List(annotationindex)
  }
  
  /** A lexical nonterminal is nullable. */
  case class NullableLexicalNonterminal(annotationindex : Int, lexical : Nonterminal) extends GrammarError
  {
    def rules = List()
    def annotations = List(annotationindex)
  }
  
  /** A nonterminal which is not lexical is used in prioritisation annotation. */
  case class NonLexicalPriority(annotationindex : Int, nonterminal : Nonterminal) extends GrammarError
  {
    def rules = List()
    def annotations = List(annotationindex)
  }
  
  def checkConstraints(grammar : Grammar) : (Vector[Rule[Int]], List[GrammarError]) = {
    import Constraints.translateConstraint
    val rules = grammar.rules
    var transformed_rules : List[Rule[Int]] = List()
    var errors : List[GrammarError] = List()
    for (ruleindex <- 0 to rules.size - 1) {
      val rule = rules(ruleindex)
      val (constraint, symbols) = translateConstraint(rule.rhs.map(_.indexedSymbol), rule.constraint, Map())
      transformed_rules = Rule(rule.lhs, rule.rhs, constraint) :: transformed_rules
      for ((symbol, indices) <- symbols) {
        val multiplicity = indices.size
        println("multiplicity = " + multiplicity)
        if (multiplicity == 0)
          errors = UndeclaredSymbolInConstraint(ruleindex, symbol) :: errors
        else if (multiplicity > 1)
          errors = AmbiguousSymbolInConstraint(ruleindex, symbol) :: errors
      }
    }
    (transformed_rules.reverse.toVector, errors.reverse)
  }
  
  def computeLexicals(grammar : Grammar) : Map[Nonterminal, Int] = {
    var lexicals : Map[Nonterminal, Int] = Map()
    val annotations = grammar.annotations
    for (annotationindex <- (annotations.size - 1) to 0 by -1) {
      val annotation = annotations(annotationindex)
      annotation match {
        case Lexical(nonterminal) =>
          lexicals = lexicals + (nonterminal -> annotationindex)
        case _ =>
      }      
    }
    lexicals
  }
  
  def checkLexicalDependencies(grammar : Grammar, lexicals : Map[Nonterminal, Int]) :
    List[GrammarError] =
  {
    val rules = grammar.rules
    var errors : List[GrammarError] = List()
    for (ruleindex <- 0 to rules.size - 1) {
      val rule = rules(ruleindex)
      if (lexicals.contains(rule.lhs)) {
        val dependencies = rule.rhs.collect {
          case AnnotatedSymbol(IndexedSymbol(x : Nonterminal, _), _) if !lexicals.contains(x) => x
        }
        for (dependency <- dependencies) {
          errors = InvalidLexicalDependency(ruleindex, lexicals(rule.lhs), 
              rule.lhs, dependency) :: errors
        }
      }
    }
    errors.reverse
  }
  
  def computeNullables(grammar : Grammar) : Set[Nonterminal] = {
    val rules = grammar.rules.filter(r => r.rhs.forall(_.indexedSymbol.symbol.isNonterminal))
    var nullables : Set[Nonterminal] = Set()
    def ruleIsNullable(rule : Rule[IndexedSymbol]) : Boolean = {
      rule.rhs.forall {
        case AnnotatedSymbol(IndexedSymbol(x : Nonterminal, _),_) => nullables.contains(x)
        case _ => false
      }
    }
    var count = nullables.size
    var changed : Boolean = true
    do {
      nullables = rules.filter(ruleIsNullable).map(_.lhs).toSet
      val newcount = nullables.size
      changed = newcount > count
      count = newcount
    } while (changed)
    nullables
  }
  
  def checkNullableLexicals(lexicals : Map[Nonterminal, Int], nullables : Set[Nonterminal]) :
    List[GrammarError] =
  {
    nullables.toList.collect {
      case x if lexicals.contains(x) => NullableLexicalNonterminal(lexicals(x), x)
    }
  }
  
  def check(grammar : Grammar) : GrammarInfo = {
    val (rules, errors1) = checkConstraints(grammar)
    val lexicals = computeLexicals(grammar)
    val errors2 = checkLexicalDependencies(grammar, lexicals)
    val nullables = computeNullables(grammar)
    val errors3 = checkNullableLexicals(lexicals, nullables)
    GrammarInfo(errors1 ++ errors2 ++ errors3, rules, lexicals, nullables)
  }
  
  case class GrammarInfo (
    errors : List[GrammarError], 
    rules : Vector[Rule[Int]],
    lexicals : Map[Nonterminal, Int],
    nullables : Set[Nonterminal])
  {
    def wellformed = errors.isEmpty
  }
  
}