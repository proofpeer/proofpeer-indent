package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TestHaskell extends Properties("Haskell") {
  val parser = Parser(HaskellGrammar.lexical)

  property("haskInteger1") = parser.parse("literal", "1234").isDefined
  property("haskHex1") = parser.parse("literal", "0x1234Ff").isDefined
  property("haskOctal1") = parser.parse("literal", "0o1234").isDefined
  property("haskOctal2") = !parser.parse("literal", "0o8").isDefined
  property("haskFloat1") = parser.parse("literal", "1234.567").isDefined
  property("haskFloat2") = parser.parse("literal", "1234E+10").isDefined
  property("haskFloat3") = parser.parse("literal", "1234E-10").isDefined
  property("haskLiteral1") = !parser.parse("literal", "1234A+10").isDefined
  property("haskChar1") = parser.parse("literal", "' '").isDefined
  property("haskChar2") = !parser.parse("literal", "'  '").isDefined
  property("haskChar3") = parser.parse("literal", "'a'").isDefined
  property("haskChar4") = !parser.parse("literal", "'\\&'").isDefined
  property("haskChar5") = parser.parse("literal", "'\\\\'").isDefined
  property("haskChar6") = parser.parse("literal", "'\\''").isDefined
  property("haskChar7") = parser.parse("literal", "'\\\"'").isDefined
  property("haskChar8") = parser.parse("literal", "'\\^Z'").isDefined
  property("haskChar9") = parser.parse("literal", "'\\^@'").isDefined
  property("haskChar10") = parser.parse("literal", "'\\NUL'").isDefined
  property("haskChar11") = parser.parse("literal", "'\\ACK'").isDefined
  property("haskChar12") = !parser.parse("literal", "'ACK'").isDefined
  property("haskChar13") = !parser.parse("literal", "''").isDefined
  property("haskString1") = parser.parse("literal","\"\"").isDefined
  property("haskString2") = parser.parse("literal","\"   \"").isDefined
  property("haskString3") = parser.parse("literal","\"hello\"").isDefined
  property("haskString4") = parser.parse("literal","\"\\&\"").isDefined
  property("haskString5") = parser.parse("literal","\"'\"").isDefined
  property("haskString6") = parser.parse("literal","\"\\\"\"").isDefined
  property("haskString7") = parser.parse("literal","\"hello world!\"").isDefined
  property("special1") = parser.parse("lexeme","(").isDefined
  property("special2") = parser.parse("lexeme",";").isDefined
  property("comment1") = parser.parse("comment","-- this is a comment").isDefined
  property("comment2") = parser.parse("comment","--").isDefined
  property("comment3") = !parser.parse("comment","--\r\nnot a comment").isDefined
  property("comment4") = parser.parse("comment","{--}").isDefined
  property("comment5") = parser.parse("comment","{-  -}").isDefined
  property("comment6") = parser.parse("comment","{-{--}-}").isDefined
  property("comment7") = parser.parse("comment","{- nested comment -}").isDefined
  property("comment8") = !parser.parse("comment","{- unclosed {- -}").isDefined
  property("comment9") =
    parser.parse("comment","{- multi\r\n {- line -}\r\n nested -}").isDefined
  property("comment10") =
    !parser.parse("comment","{- multi\r\n {- line\r\n nested unclosed -}").isDefined
  property("varid1") = parser.parse("qvarid", "foo").isDefined
  property("varid2") = parser.parse("qvarid", "Foo.foo").isDefined
  property("varid3") = !parser.parse("qvarid", "'noStartQuoting").isDefined
  property("varid4") = parser.parse("qvarid", "butCanQuoteEnd'").isDefined
  property("varid5") = !parser.parse("qvarid", "bad.qualifier").isDefined
  property("conid1") = parser.parse("qconid", "Foo").isDefined
  property("conid2") = parser.parse("qconid", "Foo.Bar").isDefined
  property("conid3") = parser.parse("qconid", "Foo.Bar.Baz").isDefined

  val doc = Document.fromString("--\r\nnot a comment")
  for ( c <- 0 to doc.size - 1) {
    System.out.println(doc.character(c))
  }
  System.out.println(parser.parse("comment","--\r\nnot a comment"))
  System.out.println(parser.parse("program", "let").map {
    c:ParseContext => c.result("lexeme")
  })
}
