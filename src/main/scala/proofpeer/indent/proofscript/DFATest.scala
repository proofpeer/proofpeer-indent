package proofpeer.indent.proofscript

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.regex.RegularExpr._
import proofpeer.indent.regex.Utils._

object Literals {

  var regexprs : List[(String, RegularExpr)] = List()
  var names : Map[Int, String] = Map()
  var lookup : Map[String, RegularExpr] = Map()
  var id = -1

  def add(s : String, expr : RegularExpr, prio : Option[Int] = None) {
    lookup += (s -> expr)
  }

  def keyword(s : String, k : String) {
    add(s, string(k))
  }

  def L(name : String) : RegularExpr = {
    lookup(name)
  }

  def dfa(terminals : Set[String]) : DFA = {
    for (name <- terminals) {
      lookup.get(name) match {
        case Some(r) => regexprs = (name, r) :: regexprs
        case None =>
      }
      
    }
    //regexprs = terminals.map(name => (name, lookup(name))).toList
    //regexprs = regexprs.reverse.filter(x => terminals.contains(x._1))
    var exprs = regexprs.reverse.map(r => {
      val name = r._1
      val e = r._2
      val result = (id, e)
      names += (id -> name)
      id -= 1
      result
    })
    println("creating DFA from "+exprs.size+" regular expressions: " + exprs)
    val nfa = NFA.fromRegularExprs(exprs)  
    nfa.display()
    val dfa = DFA.fromNFA(nfa)
    dfa.display()
    dfa
  }

  /*add("LowerLetter", chars('a', 'z'))
  add("UpperLetter", chars('A', 'Z'))
  add("Letter", ALT(L("LowerLetter"), L("UpperLetter")))
  add("Digit", chars('0', '9'))
  add("Digits", REPEAT1(L("Digit")))
  add("Underscore", char('_'))
  add("Backslash", char('\\'))
  add("Id", seq(L("Letter"), REPEAT(alt(L("Letter"), L("Digit"), 
    SEQ(L("Underscore"), L("Letter"))))), Some(1))
  add("RelativeNamespace", SEQ(REPEAT(SEQ(L("Id"), L("Backslash"))), L("Id")))
  add("AbsoluteNamespace", SEQ(L("Backslash"), L("RelativeNamespace")))
  add("Namespace", ALT(L("RelativeNamespace"), L("AbsoluteNamespace")))
  add("IndexedName", SEQ(L("Id"), OPT(SEQ(L("Underscore"), L("Digits")))), Some(1))
  add("RelativeName", SEQ(L("IndexedName"), REPEAT(SEQ(L("Backslash"), L("IndexedName")))))
  add("Name", SEQ(OPT(L("Backslash")), L("RelativeName")), Some(1))
  add("Dot", char('.'))
  add("Comma", char(','))
  add("Eq", char('='))
  add("NotEq", char(0x2260))
  add("RoundBracketOpen", char('('))
  add("RoundBracketClose", char(')'))
  add("CurlyBracketOpen", char('{'))
  add("CurlyBracketClose", char('}'))
  add("Bar", char('|'))
  add("RightArrow", char(0x2192)) 
  add("Colon", char(':')) 
  add("QuoteOpen", char(0x2039)) 
  add("QuoteClose", char(0x203A)) 
  add("Forall", char(0x2200)) 
  add("Exists", char(0x2203)) 
  add("NotExists", char(0x2204))     
  add("Universe", char(0x1D4B0)) 
  add("Prop", char(0x2119)) 
  add("Powerset", char(0x1D4AB)) 
  add("MapsTo", char(0x21A6)) 
  add("True", char(0x22A4)) 
  add("False", char(0x22A5)) 
  add("Elem", char(0x2208)) 
  add("NotElem", char(0x2209)) */
  /*add("Subset", char(0x2282)) 
  add("NotSubset", char(0x2284))     
  add("Or", char(0x2228)) 
  add("And", char(0x2227)) 
  add("Not", char(0x00AC)) 
  add("EmptySet", char(0x2205)) 
  add("SetDiff", char(0x2216)) 
  add("SetUnion", char(0x222A)) 
  add("SetIntersection", char(0x2229)) 
  add("SetBigUnion", char(0x22C3)) 
  add("SetBigIntersection", char(0x22C2))*/


  // scan rules for programming language
  //add("HexDigit", alt(chars('a', 'f'), chars('A', 'F'), chars('0', '9')))
  //add("QuotationMark", char('"'))
  /*add("StringLiteralToken", REPEAT1(alt(
    string("\\n"),
    string("\\\\"),
    string("\\\""),
    seq(L("Backslash"), char('u'), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit")),
    seq(L("Backslash"), char('U'), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit")),
    char(0x21),
    chars(0x23, 0x5B),
    chars(0x5D, 0x7E),
    chars(0xA0, Int.MaxValue))))*/
  //add("Hash", char('#')) 
  //add("AnyToken", REPEAT1(CHAR(Range.universal)))
  //add("Plus", char('+'))   
  //add("Minus", char('-')) 
  //add("Times", char('*')) 
  //add("Slash", char('/')) 
  //add("Le", char('<')) 
  //add("Gr", char('>')) 
  //add("Leq", ALT(char(0x2264), string("<="))) 
  //add("Geq", ALT(char(0x2265), string(">="))) 
  //add("SquareBracketOpen", char('[')) 
  //add("SquareBracketClose", char(']')) 
  //add("DoubleArrow", ALT(char(0x21D2), string("=>")))
  //add("ScriptEq", string("==")) 

  add("ScriptNotEq", ALT(char(0x2260), string("<>")))  
  //add("ScriptNotEq", string("<"))  
  

  //add("Apostrophe", char(0x27)) 
  //add("Prepend", string("<+")) 
  //add("Append", string("+>")) 
  //add("Concat", string("++"))
  /*keyword("Val", "val")
  keyword("Def", "def")
  keyword("Mod", "mod") 
  keyword("ScriptOr", "or") 
  keyword("ScriptAnd", "and") 
  keyword("ScriptNot", "not") 
  keyword("ScriptTrue", "true") 
  keyword("ScriptFalse", "false") 
  keyword("Lazy", "lazy") 
  keyword("If", "if") 
  keyword("Then", "then") 
  keyword("Else", "else") 
  keyword("While", "while") 
  keyword("Do", "do") 
  keyword("For", "for") 
  keyword("In", "in") 
  keyword("Match", "match") 
  keyword("Case", "case") 
  keyword("Return", "return") 
  keyword("Assume", "assume") 
  keyword("Let", "let") 
  keyword("Choose", "choose") 
  keyword("Theory", "theory") 
  keyword("Extends", "extends") 
  keyword("Context", "context") 
  keyword("Show", "show") 
  keyword("Fail", "fail") 
  keyword("Nil", "nil") 
  keyword("To", "to") 
  keyword("Downto", "downto") 
  keyword("Theorem", "theorem") 
  keyword("Assert", "assert") 
  keyword("Failure", "failure") 
  keyword("As", "as") */

}
