package proofpeer.indent

import proofpeer.indent.regex._
import proofpeer.general.StringUtils._

object GrammarSyntax {

  def string2IndexedSymbol(name : String) : IndexedSymbol = {
    val u = name.lastIndexOf("_")
    if (u >= 0) {
      val left = name.substring(0, u)
      val right = name.substring(u + 1)
      if (right == "" || !right.forall(isASCIIDigit(_)))
        IndexedSymbol(name, None)
      else 
        IndexedSymbol(left, Some(right))
    } else 
      IndexedSymbol(name, None)
  }

  private def string2rhs(s : String) : Vector[IndexedSymbol] = {
    if (s.trim().isEmpty())
      Vector()
    else
      split_nonempty(s, " ").map(string2IndexedSymbol(_)).toVector
  }

  private val letter = CHAR(Range('a', 'z') + Range('A', 'Z'))
  private val digit = CHAR(Range('0', '9'))
  private val underscore = char('_')
  private val identifier = seq(letter, REPEAT(alt(letter, digit, underscore)))

  private type Symbol = (IndexedSymbol, ParseParam)
  private type Qualifier = Constraint.LayoutQualifier

  private def r(terminal : String, regex : RegularExpr, priority : Option[Int] = None) : Grammar = 
    Grammar(ScanRule(terminal, "", priority, Lexer.untilWhitespace(regex)))

  private def r(q : String) : Grammar = r(q, string(q, false))

  private def r(nonterminal : String, rhs : String, constraint : Constraint,
    action : ParseContext => Any) : Grammar = 
  {
    val r = string2rhs(rhs)
    Grammar(ParseRule(nonterminal, r, ParseParam.noParams(r.length), constraint, 
      ParseParam.Const(earley.Earley.DEFAULT_RESULT), action))
  }

  private def r(nonterminal : String, rhs : String, action : ParseContext => Any) : Grammar = 
    r(nonterminal, rhs, Constraint.unconstrained, action)

  private def mkLIST(params : Vector[ParseParam]) : ParseParam = {
    import ParseParam._
    def mk(params : List[ParseParam]) : ParseParam = {
      if (params.isEmpty) 
        Const(NIL) 
      else
        Cons(params.head, mk(params.tail))       
    }
    if (params.size == 1) params(0) else mk(params.toList)
  }

  private val grammar = 
    r("ID", identifier) ++
    r("OPEN", char('(')) ++
    r("CLOSE", char(')')) ++
    r("CURRENT", char('~')) ++
    r("NUM", REPEAT1(digit)) ++ 
    r("MINUS", char('-')) ++
    r("PLUS", char('+')) ++
    r("BAR", char('|')) ++
    r("DOT", char('.')) ++
    r("COMMA", char(',')) ++
    r("CURLYOPEN", char('{')) ++
    r("CURLYCLOSE", char('}')) ++
    r("FIRSTROW") ++
    r("LASTROW") ++
    r("MAXROWGAP") ++
    r("LEFTMOSTINFIRST") ++
    r("LEFTMOSTFIRST") ++
    r("LEFTMOSTREST") ++
    r("LEFTMOST") ++
    r("RIGHTMOSTLAST") ++
    r("MAX") ++
    r("MIN") ++
    r("NIL") ++
    r("VAL") ++
    r("Identifier", "ID", c => string2IndexedSymbol(c.text)) ++
    r("AtomicParam", "CURRENT", c => ParseParam.Current) ++
    r("AtomicParam", "NUM", c => ParseParam.Const(ParseParam.INT(c.text.toInt))) ++
    r("AtomicParam", "NIL", c => ParseParam.Const(ParseParam.NIL)) ++
    r("AtomicParam", "OPEN ParamList CLOSE", c => mkLIST(c.ParamList[Vector[ParseParam]])) ++
    r("LayoutParam", "AtomicParam", _.AtomicParam[ParseParam]) ++
    r("LayoutParam", "Identifier DOT Qualifier", 
      c => ParseParam.LayoutEntity(c.Qualifier[Qualifier].apply(c.Identifier[IndexedSymbol]))) ++
    r("LayoutParam", "Identifier DOT VAL", c => ParseParam.VResult(c.Identifier[IndexedSymbol])) ++
    r("Qualifier", "FIRSTROW", c => Constraint.FirstRow) ++
    r("Qualifier", "LASTROW", c => Constraint.LastRow) ++
    r("Qualifier", "MAXROWGAP", c => Constraint.MaxRowGap) ++
    r("Qualifier", "LEFTMOSTINFIRST", c => Constraint.LeftMostInFirst) ++
    r("Qualifier", "LEFTMOSTFIRST", c => Constraint.LeftMostFirst) ++
    r("Qualifier", "LEFTMOST", c => Constraint.LeftMost) ++
    r("Qualifier", "LEFTMOSTREST", c => Constraint.LeftMostRest) ++
    r("Qualifier", "RIGHTMOSTLAST", c => Constraint.RightMostLast) ++
    r("DotFun", "NUM", c => (p : ParseParam) => ParseParam.Select(p, c.text.toInt)) ++
    r("DotFun", "MIN", c => (p : ParseParam) => ParseParam.Min(p)) ++
    r("DotFun", "MAX", c => (p : ParseParam) => ParseParam.Max(p)) ++
    r("DottedParam", "DottedParam DOT DotFun", 
      c => c.DotFun[ParseParam => ParseParam].apply(c.DottedParam[ParseParam])) ++
    r("DottedParam", "LayoutParam", _.LayoutParam[ParseParam]) ++
    r("NegParam", "DottedParam", _.DottedParam[ParseParam]) ++
    r("NegParam", "MINUS NegParam", c => ParseParam.Neg(c.NegParam[ParseParam])) ++
    r("AddParam", "NegParam", _.NegParam[ParseParam]) ++
    r("AddParam", "AddParam PLUS NegParam", 
      c => ParseParam.Add(c.AddParam[ParseParam], c.NegParam[ParseParam])) ++
    r("AddParam", "AddParam MINUS NegParam", 
      c => ParseParam.Sub(c.AddParam[ParseParam], c.NegParam[ParseParam])) ++
    r("AltParam", "AddParam", _.AddParam[ParseParam]) ++
    r("AltParam", "AltParam BAR AddParam", 
      c => ParseParam.Alternative(c.AltParam[ParseParam], c.AddParam[ParseParam])) ++
    r("Param", "AltParam", _.AltParam[ParseParam]) ++
    r("ParamList", "", c => Vector[ParseParam]()) ++
    r("ParamList", "Param", c => Vector[ParseParam](c.Param[ParseParam])) ++
    r("ParamList", "ParamList_1 COMMA Param", 
      Constraint.Not(Constraint.NullSpan("ParamList")),
      c => c.ParamList_1[Vector[ParseParam]] :+ c.Param[ParseParam]) ++
    r("Symbol", "Identifier", c => (c.Identifier[IndexedSymbol], ParseParam.Const(ParseParam.NIL))) ++
    r("Symbol", "Identifier AtomicParam", c => (c.Identifier[IndexedSymbol], c.AtomicParam[ParseParam])) ++
    r("Symbols", "", c => Vector[Symbol]()) ++
    r("Symbols", "Symbols Symbol", c => c.Symbols[Vector[Symbol]] :+ c.Symbol[Symbol]) ++
    r("SymbolsAndResult", "Symbols CURLYOPEN Param CURLYCLOSE",
      c => (c.Symbols[Vector[Symbol]], c.Param[ParseParam])) ++
    r("SymbolsAndResult", "Symbols", c => (c.Symbols[Vector[Symbol]], ParseParam.Const(earley.Earley.DEFAULT_RESULT)))

  private val parser = new Parser(grammar, true)
  
  private def parse[T](nonterminal : String, text : String) : Option[T] = 
    parser.parse[T](nonterminal, text)

  def parseId(name : String) : Option[IndexedSymbol] = 
    parse("Identifier", name)

  def parseParam(param : String) : Option[IndexedSymbol] = 
    parse("Param", param)

  def parseSymbol(symbol : String) : Option[Symbol] =
    parse("Symbol", symbol)

  def parseSymbols(rhs : String) : Option[Vector[Symbol]] =
    parse("Symbols", rhs)

  def parseSymbolsAndResult(rhs : String) : Option[(Vector[Symbol], ParseParam)] =
    parse("SymbolsAndResult", rhs)


}