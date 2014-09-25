package proofpeer.indent.proofscript

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.layout._
import GrammarConversions._
import Utils._
import RegularExpr._

object Syntax {

  private var scanrules : Map[String, ScanRule] = Map()
  private var parserules : Vector[ParseRule] = Vector()

  def add(name : String, r : RegularExpr, prio : Option[Int] = None) {
    val scanrule = ScanRule(name, 0, prio, r)
    if (scanrules.get(name).isDefined) throw new RuntimeException("literal already defined: " + name)
    scanrules = scanrules + (name -> scanrule)
  }

  def keyword(symbol : String, kw : String) {
    add(symbol, string(kw), Some(2))
  } 

  def L(name : String) : RegularExpr = {
    scanrules.get(name) match {
      case None => throw new RuntimeException("literal not defined: " + name)
      case Some(rule) => rule.regex
    }
  }

  def rule(symbol : String, rhs : String, constraint : Constraint = unconstrained) {
    val parserule = ParseRule(symbol, string2rhs(rhs), constraint, _ => null)
    parserules :+= parserule
  }

  def Subalign(a : String, b : String) = or(Indent(a, b), Align(a, b))

  // scan rules for logic syntax
  add("LowerLetter", chars('a', 'z'))
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
  add("NotElem", char(0x2209)) 
  add("Subset", char(0x2282)) 
  add("NotSubset", char(0x2284))     
  add("Or", char(0x2228)) 
  add("And", char(0x2227)) 
  add("Not", char(0x00AC)) 
  add("EmptySet", char(0x2205)) 
  add("SetDiff", char(0x2216)) 
  add("SetUnion", char(0x222A)) 
  add("SetIntersection", char(0x2229)) 
  add("SetBigUnion", char(0x22C3)) 
  add("SetBigIntersection", char(0x22C2))

  // parse rules for logic syntax
  // type
  rule("AtomicType", "Universe")
  rule("AtomicType", "Prop")
  rule("AtomicType", "Underscore")
  rule("AtomicType", "RoundBracketOpen Type RoundBracketClose")
  rule("Type", "AtomicType")
  rule("Type", "AtomicType RightArrow Type")
  // term (value)
  rule("ValueNameTerm", "Name")
  rule("ValueSetComprehensionTerm", "CurlyBracketOpen ValueTerm Bar ValueBindings CurlyBracketClose")
  rule("ValueSetComprehensionTerm", "CurlyBracketOpen ValueTerm_1 Bar ValueBindings Dot ValueTerm_2 CurlyBracketClose")
  rule("ValueConcreteSetTerm", "CurlyBracketOpen ValueTermList CurlyBracketClose")
  rule("ValueConcreteSetTerm", "CurlyBracketOpen CurlyBracketClose")
  rule("ValueAtomicTerm", "ValueNameTerm")
  rule("ValueAtomicTerm", "RoundBracketOpen ValueTermList RoundBracketClose")
  rule("ValueAtomicTerm", "ValueSetComprehensionTerm")
  rule("ValueAtomicTerm", "ValueConcreteSetTerm")   
  rule("ValueAtomicTerm", "True")
  rule("ValueAtomicTerm", "False")
  rule("ValueAtomicTerm", "EmptySet")
  rule("ValueAtomicTerm", "QuoteOpen ValueQuotedTerm QuoteClose")
  rule("ValueCombTerm", "ValueAtomicTerm")
  rule("ValueCombTerm", "ValueCombTerm ValueAtomicTerm")
  rule("ValuePureBinding", "IndexedName")
  rule("ValueAnnotatedBinding", "IndexedName Colon Type")
  rule("ValueAnnotatedBinding", "IndexedName Elem ValueTerm")
  rule("ValueBinding", "ValuePureBinding")
  rule("ValueBinding", "ValueAnnotatedBinding")
  rule("ValuePureBindings", "ValuePureBinding")
  rule("ValuePureBindings", "ValuePureBindings ValuePureBinding")
  rule("ValueBindings", "ValuePureBindings")
  rule("ValueBindings", "ValueAnnotatedBinding")
  rule("ValueBindings", "ValueBindings Comma ValueBinding")
  rule("ValueSetUnaryOpTerm", "ValueCombTerm")
  rule("ValueSetUnaryOpTerm", "Powerset ValueSetUnaryOpTerm")
  rule("ValueSetUnaryOpTerm", "SetBigUnion ValueSetUnaryOpTerm")
  rule("ValueSetUnaryOpTerm", "SetBigIntersection ValueSetUnaryOpTerm")
  rule("ValueSetIntersectionTerm", "ValueSetUnaryOpTerm")
  rule("ValueSetIntersectionTerm", "ValueSetIntersectionTerm SetIntersection ValueSetUnaryOpTerm")
  rule("ValueSetUnionTerm", "ValueSetIntersectionTerm")
  rule("ValueSetUnionTerm", "ValueSetUnionTerm SetUnion ValueSetIntersectionTerm")
  rule("ValueSetDiffTerm", "ValueSetUnionTerm")
  rule("ValueSetDiffTerm", "ValueSetDiffTerm SetDiff ValueSetUnionTerm")
  rule("ValueSetTerm", "ValueSetDiffTerm")
  rule("ValueSetBinaryRelationTerm", "ValueSetTerm")
  rule("ValueSetBinaryRelationTerm", "ValueSetTerm_1 Elem ValueSetTerm_2")
  rule("ValueSetBinaryRelationTerm", "ValueSetTerm_1 NotElem ValueSetTerm_2")   
  rule("ValueSetBinaryRelationTerm", "ValueSetTerm_1 Subset ValueSetTerm_2")
  rule("ValueSetBinaryRelationTerm", "ValueSetTerm_1 NotSubset ValueSetTerm_2")  
  rule("ValueTypedTerm", "ValueSetBinaryRelationTerm")
  rule("ValueTypedTerm", "ValueTypedTerm Colon Type")
  rule("ValueEqTerm", "ValueTypedTerm")
  rule("ValueEqTerm", "ValueTypedTerm_1 Eq ValueTypedTerm_2")
  rule("ValueEqTerm", "ValueTypedTerm_1 NotEq ValueTypedTerm_2")
  rule("ValueNotTerm", "Not ValueNotTerm")
  rule("ValueNotTerm", "ValueEqTerm")
  rule("ValueAndTerm", "ValueAndTerm And ValueNotTerm")
  rule("ValueAndTerm", "ValueNotTerm")
  rule("ValueOrTerm", "ValueOrTerm Or ValueAndTerm")
  rule("ValueOrTerm", "ValueAndTerm")
  rule("ValueImpliesTerm", "ValueOrTerm RightArrow ValueImpliesTerm")
  rule("ValueImpliesTerm", "ValueOrTerm")
  rule("ValuePropTerm", "ValueImpliesTerm")
  rule("ValueAbsTerm", "ValuePropTerm")
  rule("ValueAbsTerm", "ValueQuantifierTerm")
  rule("ValueQuantifierTerm", "Forall ValueBindings ValueQuantifierTerm")
  rule("ValueQuantifierTerm", "Exists ValueBindings ValueQuantifierTerm")
  rule("ValueQuantifierTerm", "NotExists ValueBindings ValueQuantifierTerm")
  rule("ValueQuantifierTerm", "Forall ValueBindings Dot ValueAbsTerm")
  rule("ValueQuantifierTerm", "Exists ValueBindings Dot ValueAbsTerm")
  rule("ValueQuantifierTerm", "NotExists ValueBindings Dot ValueAbsTerm")
  rule("ValueAbsTerm", "ValueBindings MapsTo ValueAbsTerm")
  rule("ValueTermList", "ValueTerm")
  rule("ValueTermList", "ValueTermList Comma ValueTerm")
  rule("ValueTerm", "ValueAbsTerm")
  // term (pattern)
  rule("PatternNameTerm", "Name")
  rule("PatternSetComprehensionTerm", "CurlyBracketOpen PatternTerm Bar PatternBindings CurlyBracketClose")
  rule("PatternSetComprehensionTerm", "CurlyBracketOpen PatternTerm_1 Bar PatternBindings Dot PatternTerm_2 CurlyBracketClose")
  rule("PatternConcreteSetTerm", "CurlyBracketOpen PatternTermList CurlyBracketClose")
  rule("PatternConcreteSetTerm", "CurlyBracketOpen CurlyBracketClose")
  rule("PatternAtomicTerm", "PatternNameTerm")
  rule("PatternAtomicTerm", "RoundBracketOpen PatternTermList RoundBracketClose")
  rule("PatternAtomicTerm", "PatternSetComprehensionTerm")
  rule("PatternAtomicTerm", "PatternConcreteSetTerm")  
  rule("PatternAtomicTerm", "True")
  rule("PatternAtomicTerm", "False")
  rule("PatternAtomicTerm", "EmptySet")
  rule("PatternAtomicTerm", "QuoteOpen PatternQuotedTerm QuoteClose")
  rule("PatternCombTerm", "PatternAtomicTerm")
  rule("PatternCombTerm", "PatternCombTerm PatternAtomicTerm")
  rule("PatternPureBinding", "IndexedName")
  rule("PatternAnnotatedBinding", "IndexedName Colon Type")
  rule("PatternAnnotatedBinding", "IndexedName Elem PatternTerm")
  rule("PatternBinding", "PatternPureBinding")
  rule("PatternBinding", "PatternAnnotatedBinding")
  rule("PatternPureBindings", "PatternPureBinding")
  rule("PatternPureBindings", "PatternPureBindings PatternPureBinding")
  rule("PatternBindings", "PatternPureBindings")
  rule("PatternBindings", "PatternAnnotatedBinding")
  rule("PatternBindings", "PatternBindings Comma PatternBinding")
  rule("PatternSetUnaryOpTerm", "PatternCombTerm")
  rule("PatternSetUnaryOpTerm", "Powerset PatternSetUnaryOpTerm")
  rule("PatternSetUnaryOpTerm", "SetBigUnion PatternSetUnaryOpTerm")
  rule("PatternSetUnaryOpTerm", "SetBigIntersection PatternSetUnaryOpTerm")
  rule("PatternSetIntersectionTerm", "PatternSetUnaryOpTerm")
  rule("PatternSetIntersectionTerm", "PatternSetIntersectionTerm SetIntersection PatternSetUnaryOpTerm")
  rule("PatternSetUnionTerm", "PatternSetIntersectionTerm")
  rule("PatternSetUnionTerm", "PatternSetUnionTerm SetUnion PatternSetIntersectionTerm")
  rule("PatternSetDiffTerm", "PatternSetUnionTerm")
  rule("PatternSetDiffTerm", "PatternSetDiffTerm SetDiff PatternSetUnionTerm")
  rule("PatternSetTerm", "PatternSetDiffTerm")
  rule("PatternSetBinaryRelationTerm", "PatternSetTerm")
  rule("PatternSetBinaryRelationTerm", "PatternSetTerm_1 Elem PatternSetTerm_2")
  rule("PatternSetBinaryRelationTerm", "PatternSetTerm_1 NotElem PatternSetTerm_2")
  rule("PatternSetBinaryRelationTerm", "PatternSetTerm_1 Subset PatternSetTerm_2")
  rule("PatternSetBinaryRelationTerm", "PatternSetTerm_1 NotSubset PatternSetTerm_2")   
  rule("PatternTypedTerm", "PatternSetBinaryRelationTerm")
  rule("PatternTypedTerm", "PatternTypedTerm Colon Type")
  rule("PatternEqTerm", "PatternTypedTerm")
  rule("PatternEqTerm", "PatternTypedTerm_1 Eq PatternTypedTerm_2")
  rule("PatternEqTerm", "PatternTypedTerm_1 NotEq PatternTypedTerm_2")
  rule("PatternNotTerm", "Not PatternNotTerm")
  rule("PatternNotTerm", "PatternEqTerm")
  rule("PatternAndTerm", "PatternAndTerm And PatternNotTerm")
  rule("PatternAndTerm", "PatternNotTerm")
  rule("PatternOrTerm", "PatternOrTerm Or PatternAndTerm")
  rule("PatternOrTerm", "PatternAndTerm")
  rule("PatternImpliesTerm", "PatternOrTerm RightArrow PatternImpliesTerm")
  rule("PatternImpliesTerm", "PatternOrTerm")
  rule("PatternPropTerm", "PatternImpliesTerm")
  rule("PatternAbsTerm", "PatternPropTerm")
  rule("PatternAbsTerm", "PatternQuantifierTerm")
  rule("PatternQuantifierTerm", "Forall PatternBindings PatternQuantifierTerm")
  rule("PatternQuantifierTerm", "Exists PatternBindings PatternQuantifierTerm")
  rule("PatternQuantifierTerm", "NotExists PatternBindings PatternQuantifierTerm")
  rule("PatternQuantifierTerm", "Forall PatternBindings Dot PatternAbsTerm")
  rule("PatternQuantifierTerm", "Exists PatternBindings Dot PatternAbsTerm")
  rule("PatternQuantifierTerm", "NotExists PatternBindings Dot PatternAbsTerm")
  rule("PatternAbsTerm", "PatternBindings MapsTo PatternAbsTerm")
  rule("PatternTermList", "PatternTerm")
  rule("PatternTermList", "PatternTermList Comma PatternTerm")
  rule("PatternTerm", "PatternAbsTerm")

  // scan rules for programming language
  add("HexDigit", alt(chars('a', 'f'), chars('A', 'F'), chars('0', '9')))
  add("QuotationMark", char('"'))
  add("StringLiteral", REPEAT1(alt(
    string("\\n"),
    string("\\\\"),
    string("\\\""),
    seq(L("Backslash"), char('u'), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit")),
    seq(L("Backslash"), char('U'), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit"), L("HexDigit")),
    char(0x21),
    chars(0x23, 0x5B),
    chars(0x5D, 0x7E),
    chars(0xA0, Int.MaxValue))))
  add("Hash", char('#')) 
  add("AnyToken", REPEAT1(CHAR(Range.universal)))
  add("Plus", char('+'))   
  add("Minus", char('-')) 
  add("Times", char('*')) 
  add("Slash", char('/')) 
  add("Le", char('<')) 
  add("Gr", char('>')) 
  add("Leq", ALT(char(0x2264), string("<="))) 
  add("Geq", ALT(char(0x2265), string(">="))) 
  add("SquareBracketOpen", char('[')) 
  add("SquareBracketClose", char(']')) 
  add("DoubleArrow", ALT(char(0x21D2), string("=>")))
  add("ScriptEq", string("==")) 
  add("ScriptNotEq", ALT(char(0x2260), string("<>")))  
  add("Apostrophe", char(0x27)) 
  add("Prepend", string("<+")) 
  add("Append", string("+>")) 
  add("Concat", string("++"))
  keyword("Val", "val")
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
  keyword("As", "as") 

  // parse rules for programming language   
  // expr
  rule("PrimitiveExpr", "Name")
  rule("Int", "Digits")
  rule("Int", "Minus Digits")
  rule("PrimitiveExpr", "Digits")
  rule("PrimitiveExpr", "RoundBracketOpen ExprList RoundBracketClose")
  rule("PrimitiveExpr", "SquareBracketOpen ExprList SquareBracketClose")
  rule("PrimitiveExpr", "ScriptTrue")
  rule("PrimitiveExpr", "ScriptFalse")
  rule("PrimitiveExpr", "Nil")
  rule("PrimitiveExpr", "Apostrophe ValueTerm Apostrophe")
  rule("PrimitiveExpr", "QuotationMark_1 QuotationMark_2")
  rule("PrimitiveExpr", "QuotationMark_1 StringLiteral QuotationMark_2")
  rule("OrExpr", "OrExpr ScriptOr AndExpr")
  rule("OrExpr", "AndExpr")
  rule("AndExpr", "AndExpr ScriptAnd NotExpr")
  rule("AndExpr", "NotExpr")
  rule("NotExpr", "ScriptNot NotExpr")
  rule("NotExpr", "CmpExpr")
  rule("CmpExpr", "CmpExpr CmpOp GeneralArithExpr")
  rule("CmpExpr", "GeneralArithExpr")
  rule("CmpOp", "Le")
  rule("CmpOp", "Gr")
  rule("CmpOp", "Leq")
  rule("CmpOp", "Geq")
  rule("CmpOp", "ScriptEq")
  rule("CmpOp", "ScriptNotEq")
  rule("GeneralArithExpr", "ConcatExpr")
  rule("ConcatExpr", "PrependConcatExpr")
  rule("ConcatExpr", "ConcatExpr Append ArithExpr")
  rule("PrependConcatExpr", "PrependExpr")
  rule("PrependConcatExpr", "PrependConcatExpr Concat ArithExpr")
  rule("PrependExpr", "ArithExpr Prepend PrependExpr")
  rule("PrependExpr", "ArithExpr")
  rule("ArithExpr", "RangeExpr")
  rule("RangeExpr", "AddExpr")
  rule("RangeExpr", "AddExpr_1 To AddExpr_2")
  rule("RangeExpr", "AddExpr_1 Downto AddExpr_2")
  rule("AddExpr", "AddExpr Plus NegExpr")
  rule("AddExpr", "AddExpr Minus NegExpr")
  rule("AddExpr", "NegExpr")
  rule("NegExpr", "Minus NegExpr")
  rule("NegExpr", "MultExpr")
  rule("MultExpr", "MultExpr Times BasicExpr")
  rule("MultExpr", "MultExpr Slash BasicExpr")
  rule("MultExpr", "MultExpr Mod BasicExpr")
  rule("MultExpr", "BasicExpr")
  rule("BasicExpr", "AppExpr")
  rule("AppExpr", "PrimitiveExpr")
  rule("AppExpr", "AppExpr PrimitiveExpr")
  rule("LazyExpr", "OrExpr")
  rule("LazyExpr", "Lazy LazyExpr")
  rule("FunExpr", "Pattern DoubleArrow Block")
  rule("FunExpr", "LazyExpr")
  rule("Expr", "FunExpr")
  rule("ExprList", "")
  rule("ExprList", "ExprList1")
  rule("ExprList1", "PExpr")
  rule("ExprList1", "Comma PExpr")
  rule("ExprList1", "Comma")
  rule("ExprList1", "ExprList1 Comma PExpr")
  rule("ExprList1", "ExprList1 Comma")
  rule("PExpr", "Expr")
  rule("PExpr", "ControlFlowExpr")
  // do
  rule("STDo", "Do Block",
      Indent("Do", "Block"))
  rule("DoExpr", "Do Block")
  rule("STDo", "Do Times Block",
      and(Indent("Do", "Times"), Indent("Do", "Block")))
  rule("DoExpr", "Do Times Block")
  // if
  rule("STIf", "If PExpr Then Block_1 Else Block_2", 
      and(
          Indent("If", "PExpr"), 
          ifThenElse(Line("If", "Then"), 
              and(
                  Indent("If", "Block_1"),
                  Subalign("If", "Else")),
              and(
                  Subalign("If", "Then"),
                  Indent("Then", "Block_1"),
                  or(Line("Then", "Else"), Align("Then", "Else")))), 
          ifThenElse(Line("If", "Else"), 
              Indent("If", "Block_2"),
              ifThenElse(Line("Then", "Else"), 
                Indent("Then", "Block_2"),
                Indent("Else", "Block_2")))))
  rule("STIf", "If PExpr Then Block",
      and(
          Indent("If", "PExpr"), 
          ifThenElse(Line("If", "Then"), 
              Indent("If", "Block"), 
              and(
                  Subalign("If", "Then"),
                  Indent("Then", "Block")))))
  rule("IfExpr", "If PExpr Then Block_1 Else Block_2")
  rule("IfExpr", "If PExpr Then Block")
  // while  
  rule("STWhile", "While PExpr Do Block",
      and(
        Indent("While", "PExpr"),
        ifThenElse(Line("While", "Do"),
            Indent("While", "Block"),
            and(Subalign("While", "Do"), Indent("Do", "Block")))))
  rule("WhileExpr", "While PExpr Do Block")
  // for
  rule("STFor", "For Pattern In PExpr Do Block",
      and(
          Indent("For", "Pattern"),
          Indent("For", "In"),
          Indent("For", "PExpr"),
          ifThenElse(Line("For", "Do"),
              Indent("For", "Block"),
              and(Subalign("For", "Do"), Indent("Do", "Block")))))
  rule("ForExpr", "For Pattern In PExpr Do Block")
  // match
  rule("STMatch", "Match PExpr STMatchCases",
      and(
          Indent("Match", "PExpr"),
          Subalign("Match", "STMatchCases")))
  rule("STMatchCases", "STMatchCases STMatchCase", 
      or(Align("STMatchCases", "STMatchCase"), Line("STMatchCases", "STMatchCase")))
  rule("STMatchCases", "")
  rule("STMatchCase", "Case Pattern DoubleArrow Block", 
      and(
        Indent("Case", "Pattern"),
        SameLine("Pattern", "DoubleArrow"),
        Indent("Case", "Block")))     
  rule("MatchExpr", "Match PExpr MatchCases")
  rule("MatchCases", "MatchCases MatchCase")
  rule("MatchCases", "")
  rule("MatchCase", "Case Pattern DoubleArrow Block")
  // context
  rule("STContext", "Context OptContextParam Block",
      and(
        Indent("Context", "OptContextParam"),
        Indent("Context", "Block")))
  rule("ContextExpr", "Context OptContextParam Block")
  rule("OptContextParam", "")
  rule("OptContextParam", "Le PExpr Gr")
  // controlflow
  rule("STControlFlow", "STDo")
  rule("STControlFlow", "STIf")
  rule("STControlFlow", "STWhile")
  rule("STControlFlow", "STFor")
  rule("STControlFlow", "STMatch")
  rule("STControlFlow", "STContext")
  rule("ControlFlowExpr", "DoExpr")
  rule("ControlFlowExpr", "IfExpr")
  rule("ControlFlowExpr", "WhileExpr")
  rule("ControlFlowExpr", "ForExpr")
  rule("ControlFlowExpr", "MatchExpr")
  rule("ControlFlowExpr", "ContextExpr")
  // pattern
  rule("AtomicPattern", "Underscore")
  rule("AtomicPattern", "Nil")
  rule("AtomicPattern", "IndexedName")
  rule("AtomicPattern", "Int")
  rule("AtomicPattern", "QuotationMark_1 QuotationMark_2")
  rule("AtomicPattern", "QuotationMark_1 StringLiteral QuotationMark_2")
  rule("AtomicPattern", "ScriptTrue")
  rule("AtomicPattern", "ScriptFalse")
  rule("AtomicPattern", "Apostrophe PatternTerm Apostrophe")
  rule("AtomicPattern", "RoundBracketOpen PatternList RoundBracketClose")
  rule("AtomicPattern", "SquareBracketOpen PatternList SquareBracketClose")
  rule("PrependPattern", "AtomicPattern Prepend PrependPattern")
  rule("PrependPattern", "AppendPattern")
  rule("AppendPattern", "AppendPattern Append AtomicPattern")
  rule("AppendPattern", "AtomicPattern")
  rule("AsPattern", "PrependPattern")
  rule("AsPattern", "AsPattern As IndexedName")
  rule("IfPattern", "AsPattern")
  rule("IfPattern", "IfPattern If Expr")
  rule("Pattern", "IfPattern")
  rule("OptPattern", "")
  rule("OptPattern", "Pattern")
  rule("PatternList", "")
  rule("PatternList", "PatternList1")
  rule("PatternList1", "Comma Pattern")
  rule("PatternList1", "Comma")  
  rule("PatternList1", "Pattern")
  rule("PatternList1", "PatternList1 Comma Pattern")
  rule("PatternList1", "PatternList1 Comma")
  // comment
  rule("Comment", "CommentText")
  rule("CommentText", "Hash")
  rule("CommentText", "CommentText AnyToken", Indent("CommentText", "AnyToken"))
  rule("ST", "Comment")
  // show
  rule("ST", "Show PExpr",
    Indent("Show", "PExpr"))
  // fail
  rule("ST", "Fail")
  rule("ST", "Fail PExpr",
    Indent("Fail", "PExpr"))
  // val
  rule("ST", "Val Pattern Eq Block",
    and(
      Indent("Val", "Pattern"),
      SameLine("Pattern", "Eq"),
      or(Line("Eq", "Block"), Indent("Val", "Block"))))
  rule("ST", "Val IdList", 
    Indent("Val", "IdList"))
  rule("IdList", "IndexedName")
  rule("IdList", "IdList IndexedName")
  // assign
  rule("ST", "Pattern Eq Block",
      and(
          SameLine("Pattern", "Eq"),
          Protrude("Pattern"),
          or(Line("Eq", "Block"), Indent("Pattern", "Block"))))
  // def
  rule("ST", "Def DefCases",
      Indent("Def", "DefCases"))
  rule("ST", "Def IndexedName Pattern Eq Block", 
      and(
        SameLine("Def", "IndexedName"),
        Indent("Def", "Pattern"),
        Indent("Def", "Eq"),
        Indent("Def", "Block"), 
        not(SameLine("Def", "Block"))))
  rule("DefCases", "")
  rule("DefCases", "DefCases DefCase", 
      Align("DefCases", "DefCase"))
  rule("DefCase", "IndexedName Pattern Eq Block",
      and(
          Indent("IndexedName", "Pattern"),
          //or(SameLine("Pattern", "Eq"), SameLine("IndexedName", "Eq")),
          Indent("IndexedName", "Block")))
  // return
  rule("ST", "Return PExpr", Indent("Return", "PExpr"))
  rule("ST", "Return")
  // assume
  rule("ST", "Assume OptAssign PrimitiveExpr", 
    and(
      Indent("Assume", "OptAssign"),
      Indent("Assume", "PrimitiveExpr")))
  // let
  rule("ST", "Let OptAssign PrimitiveExpr",
    and(
      Indent("Let", "OptAssign"),
      Indent("Let", "PrimitiveExpr")))
  // choose
  rule("ST", "Choose OptAssign PrimitiveExpr Block",
    and(
      Indent("Choose", "OptAssign"),
      Indent("Choose", "PrimitiveExpr"),
      Indent("Choose", "Block"))) 
  // theorem
  rule("ST", "Theorem OptAssign PrimitiveExpr Block",
    and(
      Indent("Theorem", "OptAssign"),
      Indent("Theorem", "PrimitiveExpr"),
      Indent("Theorem", "Block")))
  // logic statements
  rule("OptAssign", "")
  rule("OptAssign", "IndexedName Colon")
  // test
  rule("ST", "Assert PExpr", Indent("Assert", "PExpr"))
  rule("ST", "Failure Block", Indent("Failure", "Block"))
  // statement
  rule("Statement", "Expr", 
    or(Protrude("Expr"), not(First("Expr"))))
  rule("Statement", "ST")
  rule("Statement", "STControlFlow")
  rule("Statements", "")
  rule("Statements", "Statements Statement", Align("Statements", "Statement"))
  rule("Block", "Statements")
  // header
  rule("ST", "Theory Namespace AliasList Extends NamespaceList", 
    and(
      Indent("Theory", "Namespace"),
      ifThenElse(Line("Theory", "Extends"),
        Indent("Theory", "NamespaceList"),
        and(Align("Theory", "Extends"), Indent("Extends", "NamespaceList")))))
  rule("NamespaceList", "")
  rule("NamespaceList", "NamespaceList Namespace")
  rule("AliasList", "")
  rule("AliasList", "AliasList Alias", 
    Align("AliasList", "Alias"))
  rule("Alias", "IndexedName Eq Namespace")
  // prog
  rule("ValueQuotedTerm", "PExpr")
  rule("PatternQuotedTerm", "Pattern")
  rule("Prog", "Block")

}

object Test {

  def main(args : Array[String]) {
        
  }

}
