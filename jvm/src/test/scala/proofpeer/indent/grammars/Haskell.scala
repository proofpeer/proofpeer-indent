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
  val symbol= oneOf(Range(_),"/+-*!")
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

  val reservedid = {
    val regexp =
      List(
        "case","class","data","default","deriving","do","else","if","import","in",
        "infix","infixl","infixr","instance","let","module","newtype","of","then",
        "type","where","_").foldMap(string(_))
    lex("reservedid", regexp, Some(1))
  }

  val reservedop = {
    val regexp =
      List("..","<-","->").foldMap(string(_)) |+| oneOf(char(_),":=\\|@~")

    lex("reservedop", regexp, Some(1))
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
      simplerule("lexeme", "reservedop") ++
      simplerule("lexeme", "reservedid") ++
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
      simplerule("anythings", "anything") ++
      simplerule("anythings", "anything anythings") ++
      simplerule("ncomment", "opencom closecom") ++
      simplerule("ncomment", "opencom comcontent closecom") ++
      simplerule("comcontent",  "notcomdelim comcontent") ++
      simplerule("comcontent",  "notcomdelim") ++
      simplerule("notcomdelim", "ncomment") ++
      simplerule("notcomdelim", "notcomdelimlex") ++
      simplerule("qvarid", "conid qualsep qvarid") ++
      simplerule("qvarid", "varid") ++
      simplerule("qconid", "conid qualsep qconid") ++
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
      reservedid ++
      reservedop ++
      rule(
        "whitechar",
        "singlequote_0 singlequote_1",
        sep1("singlequote_0","singlequote_1"),
        noaction) ++
      simplerule("string", "doublequote doublequote") ++
      simplerule("string", "doublequote stringcontent doublequote") ++
      simplerule("stringcontent", "graphics") ++
      simplerule("stringcontent", "graphics stringcontent") ++
      lex("special", CHAR(special)) ++
      lex("dashes", seq(string("--"), REPEAT(char('-')))) ++
      lex("anything", anything) ++
      lex("opencom", string("{-")) ++
      lex("closecom", string("-}")) ++
      lex("notcomdelimlex", REPEAT1(CHAR(-(Range('{') + Range('-'))))) ++
      lex("varid", lowerid, Some(0)) ++
      lex("conid", conid) ++
      lex("varsym", seq(CHAR(symbol), REPEAT(CHAR(symbol + Range(':')))), Some(0)) ++
      lex("consym", seq(char(':'), REPEAT(CHAR(symbol + Range(':'))))) ++
      lex("tyvar", lowerid, Some(0)) ++
      lex("tycon", conid) ++
      lex("tycls", conid) ++
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

  def sep1(A: IndexedSymbol, B: IndexedSymbol) =
    CS.and(CS.SameLine(A,B), CS.Eq(CS.RightMostLast(A), CS.LeftMostFirst(B), -2))

  def lex(symbol: String, regexp: RegularExpr, priority:Option[Int]=None) =
    Grammar(ScanRule(symbol, "default", priority, regexp))

  def simplerule(symbol: String, rhs: String) =
    rule(symbol, rhs, noaction)

    // \(\Sw+\)   "\1"
//  def haskellLex =

    // lex("qvarid", 
    // lex("special",
    //   List(',',';','`').foldMap(regex.char(_)) |+| REPEAT(regex.char('|'))) ++
    // lex("foo", EMPTY)

}
