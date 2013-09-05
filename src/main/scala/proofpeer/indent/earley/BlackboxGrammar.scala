package proofpeer.indent.earley

import proofpeer.indent.API.{Terminal, Nonterminal, Symbol}
import scala.collection.immutable._

trait Document {
  def size : Int
  def getToken(position : Int) : Token
}

trait Token {
  def terminal : Terminal  
}

trait Rule {
  def rhsSize : Int
  def rhsAt(i : Int) : Symbol
}

trait BlackboxGrammar[Value, IntermediateValue] {
  
  /** Does nonterminal have specialised parsing routines associated with it? */
  def isBlackbox(nonterminal : Nonterminal) : Boolean
  
  /** Returns true if there are any nonterminals for which [[isBlackbox]] yields true. */
  def hasBlackboxes : Boolean
  
  /** Returns all rules associated with the given nonterminals. 
    * Note that blackboxes are also allowed to have rules.
    */
  def rulesOfNonterminal(nonterminal : Nonterminal) : Vector[Rule]
  
  /** Invokes the specialised parsing routines for a given set of blackbox nonterminals.
    * @param document The document to be parsed.
    * @param i The starting position (inclusive) from which to parse.
    * @param j The maximal end position (exclusive) of the parse.
    * @param blackboxes The set of starting nonterminals for this parse; for all of the nonterminals in the set
    *   [[isBlackbox]] will be true. 
    * @return A map from successfully parsed nonterminals to nonempty sequences of parse results. Each parse result
    *    consists of the end position (exclusive) of the parse, and the value associated with the parse.
    */
  def callBlackboxes(document : Document, i : Int, j : Int, blackboxes : Set[Nonterminal]) :
    Map[Nonterminal, Seq[(Int, Value)]]
  
  /** The value associated with a token. */
  def valueOfToken(token : Token) : Value
  
  /** The initial value for parsing a rule.
    * @param the document which is parsed
    * @param i the position in the document at which the parsing starts
    * @param nonterminal the nonterminal for which we are trying to find a parse
    * @param ruleindex the rule we are using to parse; given as index into [[rulesOfNonterminal]](nonterminal)
    * @return the initial value
    */
  def initialValue(document : Document, i : Int, nonterminal : Nonterminal, ruleindex : Int) : IntermediateValue
  
  /** Computes the next intermediate value during parsing a rule N => v . x w
    * @param document the document which is parsed
    * @param i starting position of v (inclusive)
    * @param j end position of v (exclusive); starting position of x (inclusive)
    * @param k end position of x (exclusive)
    * @param nonterminal denotes N
    * @param ruleindex the rule we are using to parse; given as index into [[rulesOfNonterminal]](nonterminal)
    * @param dot the current progress in the rule (starting with 0)
    * @param intermediateValue the intermediate value associated with v
    * @param symbolValue the value associated with x
    * @return None if the rule cannot be progressed this way; 
    *   otherwise Some(value) where value is the next intermediate value
    */
  def nextValue(document : Document, i : Int, j : Int, k : Int, nonterminal : Nonterminal, ruleindex : Int,
    dot : Int, intermediateValue : IntermediateValue, symbolValue : Value) : Option[IntermediateValue]
  
  /** Returns the value associated with the nonterminal after the given rule has been parsed successfully.
   *  The extension of the successful parse is from i (inclusive) to j (exclusive).
   */
  def finalValue(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int,
    value : IntermediateValue) : Value
    
  /** Merges two values that belong to the same earley item into a new value that subsumes both values.
    * Both values are the result of successful (partial) parses of document from i to j.
    * @return None if the result would be equivalent to value1, other Some(mv) where mv is the merged value.
    */
  def mergeValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, ruleindex : Int, dot : Int,
    value1 : IntermediateValue, value2 : IntermediateValue) : Option[IntermediateValue]
    
  /** Joins two values that were computed for the same nonterminal over the same token range. */
  def joinValues(document : Document, i : Int, j : Int, nonterminal : Nonterminal, 
    value1 : Value, value2 : Value) : Value
    
}