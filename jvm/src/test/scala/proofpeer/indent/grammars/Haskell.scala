package proofpeer.indent

import proofpeer.general.StringUtils
import proofpeer.indent.regex._
import proofpeer.indent.{Constraint => CS, _}

import scalaz._
import Scalaz.{ char => _, _ }

object HaskellGrammar {
  implicit object RegularExprIsMonoid extends Monoid[RegularExpr] {
    override def append(x: RegularExpr,y: =>RegularExpr) = ALT(x,y)
    override def zero = NOTHING
  }
  implicit object RangeIsMonoid extends Monoid[Range] {
    override def append(x: Range,y: =>Range) = x + y
    override def zero = Range.empty
  }
  implicit object GrammarIsMonoid extends Monoid[Grammar] {
    override def append(x: Grammar,y: =>Grammar) = x ++ y
    override def zero = new Grammar(Set())
  }

  def intersperse[A](xs: List[A], sep:A): List[A] =
    xs match {
      case List()  => List()
      case List(x) => List(x)
      case x::xs   => x::sep::intersperse(xs,sep)
    }

  def oneOf[M: Monoid](f: Char => M, xs: String): M = xs.toList foldMap f

  val large  = Range('A','Z')
  val small  = Range('a','z')
  val digit  = Range('0','9')
  val octit  = Range('0','7')
  val hexit  = Range('0','9') + Range('a','f') + Range('A','F')

  val decimal: RegularExpr = REPEAT1(CHAR(digit))
  val octal: RegularExpr  = REPEAT1(CHAR(octit))
  val hexadecimal: RegularExpr = REPEAT1(CHAR(hexit))

  val integer: RegularExpr = (
    decimal |+|
      seq(string("0o"),octal) |+|
      seq(string("0O"),octal) |+|
      seq(string("0x"),hexadecimal) |+|
      seq(string("0X"),hexadecimal))

  val exponent: RegularExpr =
    seq(char('e') |+| char('E'), char('+') |+| char('-'), decimal)

  val float: RegularExpr =
    seq(decimal, char('.'), decimal, OPT(exponent)) |+| seq(decimal, exponent)

  val ascLarge: RegularExpr = chars('A','Z')

  val cntrl: RegularExpr = ascLarge |+| "@[]^_".toList.foldMap(char(_))

  val ascii: RegularExpr =
    seq(char('^'),cntrl) |+| List(
      "NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","BS","HT","LF","VT","FF","CR",
      "SO","SI","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB",
      "ESC","FS","GS","RS","US","SP","DEL").foldMap(string(_))

  val special = oneOf(Range(_),"(),;`[]{}")
  val symbol = oneOf(Range(_),"/+-*!") - special - oneOf(Range(_),"_:\"'")

  val graphic = (
    small + large + symbol + digit + special +
    Range(':') + Range('"') + Range('\''))

  val (charesc, stringesc) = {
    val esccharR = oneOf(Range(_),"abfnrtv\\\"'")
    val escstrR  = esccharR + Range('&')
    val escchar: RegularExpr = CHAR(esccharR)
    val escstr:  RegularExpr = CHAR(escstrR)
    val code    = (
      ascii |+| decimal |+| seq(char('o'), octal) |+|
        seq(char('x'),hexadecimal))
    (seq(char('\\'), escchar |+| code), seq(char('\\'), escstr |+| code))
  }

  val (conid: RegularExpr, lowerid: RegularExpr) = {
    val tailRegex =
      REPEAT(List(small,large,digit,Range('\'')).foldMap(CHAR(_):RegularExpr))
    (seq(CHAR(large), tailRegex), seq(CHAR(small), tailRegex))
  }

  val noaction = (x:ParseContext) => x

  val lexical = (
    simplerule("program", "lexeme program") ++
      simplerule("program", "comment program") ++
      simplerule("program", "lexeme") ++
      simplerule("program", "comment") ++
      simplerule("lexeme", "literal") ++
      simplerule("lexeme", "special") ++
      simplerule("lexeme", "qvarid") ++
      simplerule("lexeme", "qconid") ++
      simplerule("lexeme", "qvarsym") ++
      simplerule("lexeme", "qconsym") ++
      simplerule("lexeme", "qtycon") ++
      simplerule("lexeme", "qtycls") ++
      simplerule("literal", "integer") ++
      simplerule("literal", "float") ++
      simplerule("literal", "whitechar") ++
      simplerule("literal", "graphicchar") ++
      simplerule("literal", "string") ++
      simplerule("comment", "lineComment") ++
      simplerule("comment", "ncomment") ++
      rule("lineComment", "dashes", CS.Line("dashes"), noaction) ++
      rule("lineComment", "dashes anythings",
        CS.SameLine("dashes", "anythings"), noaction) ++
      simplerule("anythings", "anything [anythings]") ++
      simplerule("ncomment", "opencom [comcontent] closecom") ++
      simplerule("comcontent",  "notcomdelim [comcontent]") ++
      simplerule("notcomdelim", "ncomment") ++
      simplerule("notcomdelim", "notcomdelimlex") ++
      simplerule("qvarid", "conid qualsep qvarid") ++
      simplerule("qvarid", "varid") ++
      rule(
        "qconid",
        "conid qualsep qconid",
        CS.and(CS.Connect("conid","qualsep"),CS.Connect("qualsep","qconid")),
        noaction) ++
      simplerule("qconid", "conid") ++
      simplerule("qvarsym", "conid qualsep qvarsym") ++
      simplerule("qvarsym", "varsym") ++
      simplerule("qconsym", "conid qualsep qconsym") ++
      simplerule("qconsym", "consym") ++
      simplerule("qtycon", "conid qualsep qtycon") ++
      simplerule("qtycon", "tycon") ++
      simplerule("qtycls", "conid qualsep qtycls") ++
      simplerule("qtycls", "tycls") ++
      simplerule("qvarsym", "varsym") ++
      rule(
        "whitechar",
        "singlequote_0 singlequote_1",
        sep1("singlequote_0","singlequote_1"),
        noaction) ++
      simplerule("string", "doublequote [stringcontent] doublequote") ++
      simplerule("stringcontent", "graphics [stringcontent]") ++
      lex("special", CHAR(special)) ++
      lex("dashes", seq(string("--"), REPEAT(char('-'))), Some(1)) ++
      lex("anything", anything) ++
      lex("opencom", string("{-")) ++
      lex("closecom", string("-}")) ++
      lex("notcomdelimlex", REPEAT1(CHAR(-(Range('{') + Range('-'))))) ++
      lex("varid", lowerid) ++
      lex("conid", conid) ++
      lex("varsym", seq(CHAR(symbol), REPEAT(CHAR(symbol + Range(':'))))) ++
      lex("consym", seq(char(':'), REPEAT(CHAR(symbol + Range(':'))))) ++
      lex("tyvar", lowerid) ++
      lex("tycon", conid) ++
      lex("tycls", conid) ++
      // Strictly Haskell98, which does not have hierarchical module names
      lex("modid", conid) ++
      lex("qualsep", char('.')) ++
      lex("integer", integer) ++
      lex("float", float) ++
      lex("graphicchar", seq(
        char('\''),
        (charesc |+| CHAR(graphic - Range('\''))),
        char('\''))) ++
      lex("graphics", stringesc |+| REPEAT1(CHAR(graphic - Range('\"')))) ++
      lex("singlequote", char('\'')) ++
      lex("doublequote", char('\"'))
  )

  sealed abstract class Assoc
  case object NoAssoc extends Assoc
  case object LeftAssoc extends Assoc
  case object RightAssoc extends Assoc

  def exp(i: Int) = "exp^" + i
  def lexp(i: Int) = "lexp^" + i
  def rexp(i: Int) = "rexp^" + i
  def qop(assoc: Assoc, i: Int) =
    "qop^(" +
    (assoc match {
      case NoAssoc => "n"
      case LeftAssoc => "l"
      case RightAssoc => "r"
    }) + "," + i + ")"

  def withCtx(
    nonterminal: String,
    nonterminalReps: String*) = {

    // Rule of the form:
    // nonterminals[i] => nonterminal[i] [semis nonterminals[i]]
    val nonterminalsRepNonLayout =
      (for (
        nonterminalRep <- nonterminalReps.toList;
        nonterminalRepMultiple = nonterminalRep + "s";
        rl <- List(
          simplerule(nonterminalRepMultiple, nonterminalRep),
          simplerule(
            nonterminalRepMultiple,
            nonterminalRep,
            "semis",
            nonterminalRepMultiple)))
      yield rl).suml

    // Rules of the form
    // nonterminalsLaidout[i] => nonterminal[i]
    // nonterminalsLaidout[i] => nonterminal[i] nonterminalsLaidout[i]
    //   with nonterminal[i] and nonterminalsLaidout[i] having same LeftMostFirst
    val nonterminalsRepLaidout =
      (for (
        nonterminalRep <- nonterminalReps.toList;
        nonterminalRepLaidout = nonterminalRep + "sLaidOut";
        rl <- List(
          simplerule(nonterminalRepLaidout, nonterminalRep),
          rule(nonterminalRepLaidout,
            List(nonterminalRep, nonterminalRepLaidout).mkString(" "),
            CS.Eq(
              CS.LeftMostFirst(nonterminalRep),
              CS.LeftMostFirst(nonterminalRepLaidout),
              0),
            noaction)))
      yield rl).suml

    val nonterminalSeqs =
      nonterminalReps.toList.traverse { nt => List(List(nt), List()) }.map(_.flatten)

    val nonLayout =
      (for (
        seq <- nonterminalSeqs;
        seqs = seq.map(_ ++ "s"))
      yield simplerule(nonterminal,
        "openctx", seqs.mkString(" semis "), "closectx")
      ).suml;

    val layout =
      (for (
        nt::nts   <- nonterminalSeqs;
        ntl::ntsl =  (nt::nts).map { _ + "sLaidOut" };
        constraints =
          for (ntl_ <- ntsl)
        yield CS.Eq(CS.LeftMostFirst(ntl), CS.LeftMostFirst(ntl_), 0))
      yield rule(
        nonterminal,
        (ntl::ntsl).mkString(" "),
        CS.and(constraints:_*),
        noaction)).suml

    nonterminalsRepNonLayout ++ nonterminalsRepLaidout ++ nonLayout ++ layout
  }

  def when[M:Monoid](b:Boolean)(x:M) = if (b) x else ∅[M]

  def grammar(i: Int, a: Assoc) = (
    when (i == 0 && a == NoAssoc) {
      keywords(
        "module","where","import","qualified","as","type","data","deriving",
        "if","then","else","case","of", "instance", "default",
        "infixl", "infixr", "infix", "let", "in", "do") ++
      keysyms(
        ("equals","="),
        ("implies","=>"),
        ("hastype","::"),
        ("arrow","->"),
        ("openparen", "("),
        ("closeparen", ")"),
        ("opensquare", "["),
        ("closesquare", "]"),
        ("openbrace", "{"),
        ("closebrace", "}"),
        ("semi", ";"),
        ("comma", ","),
        ("ldots", ".."),
        ("bang", "!"),
        ("vert", "|"),
        ("unaryneg", "-"),
        ("lambda", "\\"),
        ("drawfrom", "<-"),
        ("at", "@"),
        ("backquote", "`"),
        ("colon", ":")
      ) ++
      lex("plus", char('+'))
    } ++
      simplerule("openctx", "openbrace [semis]") ++
      simplerule("closectx", "[semis] closebrace") ++
      simplerule("semis", "semi [semis]") ++
      simplerule("moduledef", "module modid [exports] where [body]") ++
      simplerule("moduledef", "body") ++
      withCtx("body","impdecl","topdecl") ++
      simplerule("exports", "anexport [comma exports]") ++
      simplerule("anexport", "qvar") ++
      simplerule("anexport", "qtycon") ++
      simplerule("anexport", "qtycon openparen ldots closeparen") ++
      simplerule("anexport", "qtycon openparen cnames closeparen") ++
      simplerule("anexport", "qtycls openparen ldots closeparen") ++
      simplerule("anexport", "qtycls openparen qvars closeparen") ++
      simplerule("anexport", "module modid") ++
      simplerule("impdecl", "import [qualified] modid [as modid]" ++
        "[openparen impspec closeparen]") ++
      simplerule("impspec", "animport [comma impspec]") ++
      simplerule("animport", "var") ++
      simplerule("animport", "tycon") ++
      simplerule("animport", "tycon openparen ldots closeparen") ++
      simplerule("animport", "tycon openparen cnames closeparen") ++
      simplerule("animport", "tycls openparen ldots closeparen") ++
      simplerule("animport", "tycls openparen vars closeparen") ++
      simplerule("qvars", "qvar [qvars]") ++
      simplerule("cnames", "cname cnames") ++
      simplerule("cname",  "var") ++
      simplerule("cname",  "con") ++
      simplerule("topdecl", "type simpletype equals thetype") ++
      simplerule("topdecl", "data [context implies] simpletype equals newconstr"
        ++ "[derivingClause]") ++
      simplerule("topdecl", "class [scontext implies] tycls tyvar [where cdecls]") ++
      simplerule("topdecl", "instance [scontext implies] qtycls inst [where idecls]") ++
      simplerule("topdecl", "default typelist") ++
      simplerule("topdecl", "decl") ++
      simplerule("typelist", "thetype [comma typelist]") ++
      withCtx("decls", "decl") ++
      simplerule("decl", "gendecl") ++
      simplerule("decl", "funlhs rhs") ++
      simplerule("decl", "pat0 rhs") ++
      withCtx("cdecls", "cdecl") ++
      simplerule("cdecl", "gendecl") ++
      simplerule("cdecl", "funlhs rhs") ++
      simplerule("cdecl", "var rhs") ++
      withCtx("idecls", "idecl") ++
      simplerule("idecl", "funlhs rhs") ++
      simplerule("idecl", "var rhs") ++
      simplerule("gendecl", "vars hastype [context implies] thetype") ++
      simplerule("gendecl", "fixity [integer] ops") ++
      simplerule("ops", "op comma [ops]") ++
      simplerule("vars", "var comma [vars]") ++
      simplerule("fixity", "infixl") ++
      simplerule("fixity", "infixr") ++
      simplerule("fixity", "infix") ++
      simplerule("thetype", "btype [arrow type]") ++
      simplerule("btype", "[btype] atype") ++
      simplerule("atype", "gtycon") ++
      simplerule("atype", "tyvar") ++
      simplerule("atype", "openparen type comma types closeparen") ++
      simplerule("atype", "opensquare type closesquare") ++
      simplerule("atype", "openparen type closeparen") ++
      simplerule("types", "type [comma types]") ++
      simplerule("gtycon", "qtycon") ++
      simplerule("gtycon", "openparen closeparen") ++
      simplerule("gtycon", "opensquare closesquare") ++
      simplerule("gtycon", "openparen arrow closeparen") ++
      simplerule("gtycon", "openparen commas closeparen") ++
      simplerule("commas", "comma [commas]") ++
      simplerule("context", "class") ++
      simplerule("context", "openparen classes closeparen") ++
      simplerule("classes", "class [comma classes]") ++
      simplerule("class", "qtycls tyvar") ++
      simplerule("class", "qtycls openparen tyvar atypes closeparen") ++
      simplerule("atypes", "atype [atypes]") ++
      simplerule("scontext", "simpleclass") ++
      simplerule("scontext", "openparen simpleclasses closeparen") ++
      simplerule("simpleclasses", "simpleclass [comma simpleclasses]") ++
      simplerule("simpleclass", "qtycls tyvar") ++
      simplerule("simpletype", "tycon tyvars") ++
      simplerule("tyvars", "tyvar [tyvars]") ++
      simplerule("constrs", "constr [constrs]") ++
      simplerule("constr", "con [lazyatypes]") ++
      simplerule("constr", "btype conop btype") ++
      simplerule("constr", "btype conop bang atype") ++
      simplerule("constr", "bang atype conop btype") ++
      simplerule("constr", "bang atype conop bang atype") ++
      simplerule("con", "openbrace fielddecls closebrace") ++
      simplerule("lazyatypes", "lazyatype [lazyatypes]") ++
      simplerule("lazyatype", "[openparen bang closeparen] atype") ++
      simplerule("newconstr", "con atype") ++
      simplerule("newconstr", "con openbrace var hastype type closebrace") ++  
      simplerule("fielddecls", "fielddecl [comma fielddecls]") ++
      simplerule("fielddecl", "vars hastype thetype") ++
      simplerule("fielddecl", "vars hastype bang atype") ++
      simplerule("derivingClause", "deriving dclass") ++
      simplerule("derivingClause", "deriving openparen [dclasses] closeparen") ++
      simplerule("dclasses", "dclass [dclasses]") ++
      simplerule("dclass", "qtycls") ++
      simplerule("inst", "gtycon") ++
      simplerule("inst", "openparen qtycon [tyvars] closeparen") ++
      simplerule("inst", "openparen tyvar comma tyvar comma commatyvars closeparen") ++
      simplerule("inst", "opensquare tyvar closesquare") ++
      simplerule("inst", "tyvar arrow tyvar") ++
      simplerule("commatyvars", "tyvar [comma commatyvars]") ++
      simplerule("funlhs", "var apat openbrace apat closebrace") ++
      simplerule("funlhs", "pat" + (i + 1), "varop" + a + i, "pat" + (i + 1)) ++
      simplerule("funlhs", "lpat" + i, "varopLeftAssoc" + i, "pat" + (i + 1)) ++
      simplerule("funlhs", "pat" + (i + 1), "varopRightAssoc" + i, "rpat" + i) ++
      simplerule(
        "funlhs",
        "openparen funlhs closeparen apat openbrace apat closebrace") ++
      simplerule("rhs", "equals exp [where decls]") ++
      simplerule("rhs", "gdrhs [where decls]") ++
      simplerule("gdrhs", "gd equals exp [gdrhs") ++
      simplerule("gd", "vert exp0") ++
      simplerule("exp", "exp0 hastype [context implies] thetype") ++
      simplerule("exp", "exp0") ++
      when (i < 9) {
        simplerule("exp" + i, "exp" + (i+1), "[qopNoAssoc" + i, "exp" + (i+1) + "]") ++
        simplerule("rexp" + i, "exp" + (i+1), "qopRightAssoc" + i, "rexp" + i)
      } ++
      simplerule("lexp" + i, "lexp" + i, "qopLeftAssoc" + i, "exp" + (i+1)) ++
      simplerule("lexp" + i, "exp" + (i+1), "qopLeftAssoc" + i, "exp" + (i+1)) ++
      simplerule("rexp" + i, "exp" + (i+1), "qopRightAssoc" + i, "exp" + (i+1)) ++
      simplerule("exp" + i, "lexp" + i) ++
      simplerule("exp" + i, "rexp" + i) ++
      simplerule("lexp6", "unaryneg exp7") ++
      simplerule("exp10", "lambda apats arrow exp") ++
      simplerule("exp10", "let decls in exp") ++
      simplerule("exp10", "if exp then exp else exp") ++
      simplerule("exp10", "case exp of alts") ++
      simplerule("exp10", "do stmts") ++
      simplerule("exp10", "fexp") ++
      simplerule("apats", "apat [apats]") ++
      simplerule("fexp", "[fexp] aexp") ++
      simplerule("aexp", "qvar") ++
      simplerule("aexp", "gcon") ++
      simplerule("aexp", "literal") ++
      simplerule("aexp", "openparen exp closeparen") ++
      simplerule("aexp", "openparen exp comma tupleexps closeparen") ++
      simplerule("aexp", "opensquare listexps closesquare") ++
      simplerule("aexp", "opensquare [comma exp] ldots [exp] closesquare") ++
      simplerule("aexp", "opensquare exp vert quals closesquare") ++
      when (i < 9) {
        simplerule("aexp", "openparen", "exp" + (i+1), "qop" + a + i, "closeparen") ++
        simplerule(
          "aexp", "openparen", "qop" + a + i, "exp" + (i+1), "closeparen") ++
        simplerule("aexp", "qopRightAssoc" + i, "rexp" + (i+1)) ++
        // Differs slightly from the Report, which expresses this with a difference
        simplerule("qop", "unaryneg")
      } ++
      simplerule("aexp", "openparen", "lexp" + i, "qopLeftAssoc" + i, "closeparen") ++
      simplerule("aexp", "qcon openbrace [fbinds] closebrace") ++
      // Differs slightly from the Report, which expresses this with a difference
      simplerule("aexp", "qvar openbrace fbinds closebrace") ++
      simplerule("tupleexps", "exp [comma tupleexps]") ++
      simplerule("listexps", "exp [comma listexps]") ++
      simplerule("quals", "qual [comma quals]") ++
      simplerule("fbinds", "fbind [comma fbinds]") ++
      simplerule("qual", "pat drawfrom exp") ++
      simplerule("qual", "let decls") ++
      simplerule("qual", "exp") ++
      withCtx("alts", "alt") ++
      simplerule("alt", "pat arrow exp [where decls]") ++
      simplerule("alt", "pat gdpat [where decls]") ++
      simplerule("gdpat", "gd arrow exp [gdpat]") ++
      withCtx("stmts", "stmt") ++
      simplerule("stmt", "exp") ++
      simplerule("stmt", "pat drawfrom exp") ++
      simplerule("stmt", "let decls") ++
      simplerule("fbind", "qvar equals exp") ++
      // Not Haskell 2010
      simplerule("pat", "var plus integer") ++
      simplerule("pat", "pat0") ++
      when (i < 9) {
        simplerule(
          "pat" + i, "pat" + (i+1), "[qconopNoAssoc" + i + " pat" + (i+1) + "]")
      } ++
      simplerule("lpat" + i, "lpat" + i, "qconopLeftAssoc" + i, "pat" + (i+1)) ++
      simplerule("lpat" + i, "pat" + (i+1), "qconopLeftAssoc" + i, "pat" + (i+1)) ++
      simplerule("rpat" + i, "pat" + (i+1), "qconopRightAssoc" + i, "rpat" + i) ++
      simplerule("rpat" + i, "pat" + (i+1), "qconopRightAssoc" + i, "pat" + (i+1)) ++
      simplerule("pat" + i, "lpat" + i) ++
      simplerule("pat" + i, "rpat" + i) ++
      simplerule("lpat6", "unaryneg integer") ++
      simplerule("lpat6", "unaryneg float") ++
      simplerule("pat10", "apat") ++
      simplerule("pat10", "gcon apats") ++
      simplerule("apat", "var [at apat]") ++
      simplerule("apat", "gcon") ++
      simplerule("apat", "gcon openbrace fpats closebrace") ++
      simplerule("fpats", "fpat [fpats]") ++
      simplerule("fpat", "qvar equals pat") ++
      simplerule("gcon", "openparen closeparen") ++
      simplerule("gcon", "opensquare closesquare") ++
      simplerule("gcon", "openparen commas closeparen") ++
      simplerule("gcon", "qcon") ++
      simplerule("var", "varid") ++
      simplerule("var", "openparen varsym closeparen") ++
      simplerule("qvar", "qvarid") ++
      simplerule("qvar", "openparen qvarsym closeparen") ++
      simplerule("con", "conid") ++
      simplerule("con", "openparen consym closeparen") ++
      simplerule("qcon", "qconid") ++
      simplerule("qcon", "openparen gconsym closeparen") ++
      simplerule("varop" + a + i, "varsym") ++
      simplerule("varop" + a + i, "backquote varid backquote") ++
      simplerule("varop", "varsym") ++
      simplerule("varop", "backquote varid backquote") ++
      simplerule("qvarop" + a + i, "qvarsym") ++
      simplerule("qvarop" + a + i, "backquote qvarid backquote") ++
      simplerule("conop", "consym") ++
      simplerule("conop", "backquote conid backquote") ++
      simplerule("qconop" + a + i, "qconsym") ++
      simplerule("qconop" + a + i, "backquote qconid backquote") ++
      simplerule("op", "varop") ++
      simplerule("op", "conop") ++
      simplerule("qop" + a + i, "qvarop" + a + i) ++
      simplerule("qop" + a + i, "qconop" + a + i) ++
      simplerule("gconsym", "colon") ++
      simplerule("gconsym", "qconsym")
  )

  def keywords(kws: String*) =
    kws.toList.foldMap { kw => lex(kw,string(kw),Some(1)) }

  def keysyms(ksyms: (String,String)*) =
    ksyms.toList.foldMap { case (name,sym) => lex(name,string(sym),Some(1)) }

  def sep1(A: IndexedSymbol, B: IndexedSymbol) =
    CS.and(CS.SameLine(A,B), CS.Eq(CS.RightMostLast(A), CS.LeftMostFirst(B), -2))

  def lex(symbol: String, regexp: RegularExpr, priority:Option[Int]=None) =
    Grammar(ScanRule(symbol, "default", priority, regexp))

  def unfoldM[M[_]:Monad,W:Monoid,A](x: A)(f: A => M[Option[(A,W)]]): M[W] = {
    f(x) >>= {
      case None          => ∅[W].point[M]
      case (Some((y,w))) => unfoldM(y)(f).map {w ⊹ _}
    }
  }

  def unfoldW[W:Monoid,A](x: A)(f: A => Option[(A,W)]): W =
    unfoldM[Id,W,A](x)(f)

  def splitOn(delims: List[Char], str:String) = {
    val positions = delims.map(str.indexOf(_)).filter(_ >= 0)
    if (positions.isEmpty)
      (None,str,"")
    else {
      val pos = positions.min
      (Some(str.charAt(pos)),str.substring(0,pos), str.substring(pos+1,str.length))
    }
  }

  def splitRhs(rhs: String) =
    (unfoldW((List('[',' '),rhs)) {
      case (_,"")    => None
      case (delims,rhs) =>
        splitOn(delims,rhs) match {
          case (None,next,_) =>
            Some((delims,""),List(next.trim))
          case (Some('['),next,rest) =>
            Some((List(']'), rest),List(next.trim))
          case (Some(']'),next,rest) =>
            Some((List('[',' '),rest), List("[" + next.trim + "]"))
          case (Some(_),next,rest) =>
            Some((delims,rest),List(next.trim))
        }
    }).filter (_ != "")

  def simplerule(nonterminal: String, rhs: String*) = {
    val rhsNts = splitRhs(rhs.toList.mkString(" ")).map {
      case nt if nt.startsWith("[") && nt.endsWith("]") =>
        List(List(nt.substring(1,nt.length - 1)), List())
      case nt => List(List(nt))
    }
    rhsNts.toList.sequence.foldMap {
      rhs =>
        System.out.println(nonterminal + " " + rhs.flatten.mkString(" "))
        rule(nonterminal,rhs.flatten.mkString(" "),noaction)
    }
  }
}
