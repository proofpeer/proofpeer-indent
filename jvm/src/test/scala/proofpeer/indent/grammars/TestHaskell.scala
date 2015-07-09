package proofpeer.indent

import scalaz._
import Scalaz._
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TestHaskell extends Properties("Haskell") {
  val lexer = Parser(HaskellGrammar.lexical)

  // property("haskInteger1") = lexer.parse("literal", "1234").isDefined
  // property("haskHex1") = lexer.parse("literal", "0x1234Ff").isDefined
  // property("haskOctal1") = lexer.parse("literal", "0o1234").isDefined
  // property("haskOctal2") = !lexer.parse("literal", "0o8").isDefined
  // property("haskFloat1") = lexer.parse("literal", "1234.567").isDefined
  // property("haskFloat2") = lexer.parse("literal", "1234E+10").isDefined
  // property("haskFloat3") = lexer.parse("literal", "1234E-10").isDefined
  // property("haskLiteral1") = !lexer.parse("literal", "1234A+10").isDefined
  // property("haskChar1") = lexer.parse("literal", "' '").isDefined
  // property("haskChar2") = !lexer.parse("literal", "'  '").isDefined
  // property("haskChar3") = lexer.parse("literal", "'a'").isDefined
  // property("haskChar4") = !lexer.parse("literal", "'\\&'").isDefined
  // property("haskChar5") = lexer.parse("literal", "'\\\\'").isDefined
  // property("haskChar6") = lexer.parse("literal", "'\\''").isDefined
  // property("haskChar7") = lexer.parse("literal", "'\\\"'").isDefined
  // property("haskChar8") = lexer.parse("literal", "'\\^Z'").isDefined
  // property("haskChar9") = lexer.parse("literal", "'\\^@'").isDefined
  // property("haskChar10") = lexer.parse("literal", "'\\NUL'").isDefined
  // property("haskChar11") = lexer.parse("literal", "'\\ACK'").isDefined
  // property("haskChar12") = !lexer.parse("literal", "'ACK'").isDefined
  // property("haskChar13") = !lexer.parse("literal", "''").isDefined
  // property("haskString1") = lexer.parse("literal","\"\"").isDefined
  // property("haskString2") = lexer.parse("literal","\"   \"").isDefined
  // property("haskString3") = lexer.parse("literal","\"hello\"").isDefined
  // property("haskString4") = lexer.parse("literal","\"\\&\"").isDefined
  // property("haskString5") = lexer.parse("literal","\"'\"").isDefined
  // property("haskString6") = lexer.parse("literal","\"\\\"\"").isDefined
  // property("haskString7") = lexer.parse("literal","\"hello world!\"").isDefined
  // property("anything") = lexer.parse("anythings","arbitrary stuff").isDefined
  property("comment1") = lexer.parse("comment","-- this is a comment").isDefined
  // property("comment2") = lexer.parse("comment","-----").isDefined
  property("comment3") = !lexer.parse("comment","-- this is\r\nnot a comment").isDefined
  // property("comment4") = lexer.parse("comment","{-# LANGUAGE CPP #-}").isDefined
  // property("comment5") = lexer.parse("comment","{-  -}").isDefined
  // property("comment6") = lexer.parse("comment","{-{--}-}").isDefined
  // property("comment7") = lexer.parse("comment","{- nested comment -}").isDefined
  // property("comment8") = !lexer.parse("comment","{- unclosed {- -}").isDefined
  // property("comment9") =
  //   lexer.parse("comment","{- multi\r\n {- line -}\r\n nested -}").isDefined
  // property("comment10") =
  //   !lexer.parse("comment","{- multi\r\n {- line\r\n nested unclosed -}").isDefined
  // property("varid1") = lexer.parse("qvarid", "foo").isDefined
  // property("varid2") = lexer.parse("qvarid", "Foo.foo").isDefined
  // property("varid3") = !lexer.parse("qvarid", "'noStartQuoting").isDefined
  // property("varid4") = lexer.parse("qvarid", "butCanQuoteEnd'").isDefined
  // property("varid5") = !lexer.parse("qvarid", "bad.qualifier").isDefined
  // property("conid1") = lexer.parse("qconid", "Foo").isDefined
  // property("conid2") = lexer.parse("qconid", "Foo.Bar").isDefined
  // property("conid3") = lexer.parse("qconid", "Foo.Bar.Baz").isDefined

  val src = scala.io.Source.fromFile("/home/phil/proofpeer/proofpeer-indent/bar.hs")
  val contents = src.mkString

  val earleyAutomaton = new earley.EarleyAutomaton(HaskellGrammar.foo)
  val earleyParser = new earley.Earley(earleyAutomaton)

  def indent(i: Int, str: String) = {
    val sb = new StringBuilder();
    for (j <- 0 to i-1) {
      sb.append(' ')
    }
    sb.append(str)
    System.out.println(sb)
  }

  def printTree(document: Document, tree: ParseTree, i: Int): Unit = {
    tree match {
      case NonterminalNode(symbol, _, _, rhs, value) =>
        indent(i, symbol)
        for (t <- rhs)
          printTree(document, t, i+2)
      case TerminalNode(symbol, span) =>
        indent(i, symbol + ": " + document.getText(span))
      case AmbiguousNode(_, _, alts) =>
        indent(i,"Ambiguously")
        for (t <- alts)
          printTree(document, t, i+2)
    }
  }

  def printRule(rule: Rule) = {
    rule match {
      case ParseRule(symbol, rhs, includes, constraint, _) =>
        System.out.println(symbol + ": " + rhs.mkString(" ") + " " + includes + " " + constraint)
      case ScanRule(symbol, _, priority, regex) =>
        System.out.println(symbol + ": " +
          priority.map(_.toString + " ").getOrElse(""))
    }
  }

  for (rl <- HaskellGrammar.foo.rules)
    printRule(rl)

  // Parse, printing out fixities.
  val doc = Document.fromString(contents)
  earleyParser.parse(doc, "moduledef") match {
    case Left(p) => {
      System.out.println("OK, parsed with fixities: " +
        HaskellGrammar.findNodes(p)(_.symbol == "gendecl").flatMap {
          tree => tree.getValue[List[HaskellGrammar.FixityDecl]]
        });
      printTree(doc,p,0)
    }
    case Right(n) =>
      System.out.println("Failed around: " + doc.character(n-1))
  }
}
