package proofpeer.indent

import scalaz._
import Scalaz._
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TestHaskell extends Properties("Haskell") {
  val lexer = Parser(HaskellGrammar.lexical)

  property("haskInteger1") = lexer.parse("literal", "1234").isDefined
  property("haskHex1") = lexer.parse("literal", "0x1234Ff").isDefined
  property("haskOctal1") = lexer.parse("literal", "0o1234").isDefined
  property("haskOctal2") = !lexer.parse("literal", "0o8").isDefined
  property("haskFloat1") = lexer.parse("literal", "1234.567").isDefined
  property("haskFloat2") = lexer.parse("literal", "1234E+10").isDefined
  property("haskFloat3") = lexer.parse("literal", "1234E-10").isDefined
  property("haskLiteral1") = !lexer.parse("literal", "1234A+10").isDefined
  property("haskChar1") = lexer.parse("literal", "' '").isDefined
  property("haskChar2") = !lexer.parse("literal", "'  '").isDefined
  property("haskChar3") = lexer.parse("literal", "'a'").isDefined
  property("haskChar4") = !lexer.parse("literal", "'\\&'").isDefined
  property("haskChar5") = lexer.parse("literal", "'\\\\'").isDefined
  property("haskChar6") = lexer.parse("literal", "'\\''").isDefined
  property("haskChar7") = lexer.parse("literal", "'\\\"'").isDefined
  property("haskChar8") = lexer.parse("literal", "'\\^Z'").isDefined
  property("haskChar9") = lexer.parse("literal", "'\\^@'").isDefined
  property("haskChar10") = lexer.parse("literal", "'\\NUL'").isDefined
  property("haskChar11") = lexer.parse("literal", "'\\ACK'").isDefined
  property("haskChar12") = !lexer.parse("literal", "'ACK'").isDefined
  property("haskChar13") = !lexer.parse("literal", "''").isDefined
  property("haskString1") = lexer.parse("literal","\"\"").isDefined
  property("haskString2") = lexer.parse("literal","\"   \"").isDefined
  property("haskString3") = lexer.parse("literal","\"hello\"").isDefined
  property("haskString4") = lexer.parse("literal","\"\\&\"").isDefined
  property("haskString5") = lexer.parse("literal","\"'\"").isDefined
  property("haskString6") = lexer.parse("literal","\"\\\"\"").isDefined
  property("haskString7") = lexer.parse("literal","\"hello world!\"").isDefined
  property("special1") = lexer.parse("lexeme","(").isDefined
  property("special2") = lexer.parse("lexeme",";").isDefined
  property("comment1") = lexer.parse("comment","-- this is a comment").isDefined
  property("comment2") = lexer.parse("comment","--").isDefined
  property("comment3") = !lexer.parse("comment","--\r\nnot a comment").isDefined
  property("comment4") = lexer.parse("comment","{--}").isDefined
  property("comment5") = lexer.parse("comment","{-  -}").isDefined
  property("comment6") = lexer.parse("comment","{-{--}-}").isDefined
  property("comment7") = lexer.parse("comment","{- nested comment -}").isDefined
  property("comment8") = !lexer.parse("comment","{- unclosed {- -}").isDefined
  property("comment9") =
    lexer.parse("comment","{- multi\r\n {- line -}\r\n nested -}").isDefined
  property("comment10") =
    !lexer.parse("comment","{- multi\r\n {- line\r\n nested unclosed -}").isDefined
  property("varid1") = lexer.parse("qvarid", "foo").isDefined
  property("varid2") = lexer.parse("qvarid", "Foo.foo").isDefined
  property("varid3") = !lexer.parse("qvarid", "'noStartQuoting").isDefined
  property("varid4") = lexer.parse("qvarid", "butCanQuoteEnd'").isDefined
  property("varid5") = !lexer.parse("qvarid", "bad.qualifier").isDefined
  property("conid1") = lexer.parse("qconid", "Foo").isDefined
  property("conid2") = lexer.parse("qconid", "Foo.Bar").isDefined
  property("conid3") = lexer.parse("qconid", "Foo.Bar.Baz").isDefined

  import HaskellGrammar.GrammarIsMonoid
  val allprods =
    for (
      i <- 0 to 9;
      a <- List(
        HaskellGrammar.LeftAssoc,
        HaskellGrammar.RightAssoc,
        HaskellGrammar.NoAssoc))
    yield HaskellGrammar.grammar(i,a)

  Parser(allprods.toList.foldMap(x => x) ++ HaskellGrammar.lexical)

//  val parser = Parser(HaskellGrammar.grammar(0) ++ HaskellGrammar.lexical)

  property("bar") =
    Parser(
      HaskellGrammar.lex("thing",regex.char('X')) ++
        HaskellGrammar.lex("comma",regex.char(',')) ++
        HaskellGrammar.simplerule("things", "thing [comma things]")).parse("things",
      "X, X, X"
    ).isDefined

  // property("foo") = parser.parse("moduledef",
  //   "module Foo where\r\n" ++
  //     "  import Bar\r\n  import Baz\r\n  import Boo").isDefined

}
