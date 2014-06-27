package proofpeer.indent.test

import proofpeer.indent._
import proofpeer.indent.test.Example.{check => test}
import proofpeer.indent.test.Example._

import API._
import APIConversions._
import Constraints._
import proofpeer.indent.Derivation

import org.scalacheck.Properties

object TestAPI extends Properties("API") {
    
  property("example0") = test(g_prog2, "a 1", "E", null)
  property("example1") = test(g_prog2, ex1, "E", result1)  
  property("example2") = test(g_prog2, ex2, "E", result2)  
  property("example3") = test(g_prog2, ex3, "E", result3)  
  property("example5") = test(g_prog1, ex5, "E", result5)  
  property("example6") = test(g_prog3, ex6, "E", result6)  
  property("example7") = test(g_prog3, ex7, "E", result7)  
  property("example8") = test(g_prog3, ex8, "E", result8)  
  property("wellformed1") = rule("A", "B C", SameLine("B", "C")).info.wellformed
  property("wellformed2") = rule("A", "B 0", SameLine("B", "0")).info.wellformed
  property("wellformed3") = rule("A", "B -", SameLine("B", "-")).info.wellformed
      

  val p = LexicalPriority(0, None)

  val g_comments = 
    lexrule("Anything", "AnythingS", p) ++
    lexrule("Anything", "AnythingNS", p) ++
    rule("AnythingS", "Anything STAR") ++ 
    rule("AnythingNS", "") ++
    rule("AnythingNS", "AnythingNS NoStar") ++
    rule("AnythingNS", "AnythingS NoStarOrSlash", SameLine("AnythingS", "NoStarOrSlash")) ++
    rule("AnythingNS", "AnythingS NoStar", Not(SameLine("AnythingS", "NoStar"))) ++
    tokenrule("NoStarOrSlash", Range.add(Range.outside_interval(42, 47), Range.interval(43, 46))) ++
    tokenrule("NoStar", Range.outside_interval(42, 42)) ++
    tokenrule("STAR", Range.singleton(42)) ++
    lexical("STAR", p) ++
    lexical("NoStarOrSlash", p) ++ 
    lexical("NoStar", p) ++ 
    lexical("AnythingS", p) ++ 
    lexical("AnythingNS", p) ++
    lexrule("Comment", literal("//"), p) ++
    lexrule("Comment", literal("##"), p) ++
    rule("Comment", "Comment -", SameLine("Comment", "-")) ++
    lexrule("OpenBlockComment", literal("/*"), p) ++
    lexrule("CloseBlockComment", literal("*/"), p) ++ 
    rule("BlockComment", "OpenBlockComment Anything CloseBlockComment") ++
    lexical("BlockComment", p) ++
    lexrule("AnyComment", "Comment", p) ++
    lexrule("AnyComment", "BlockComment", p) ++
    lexrule("WS", "", p) ++
    rule("WS", "WS AnyComment", Less[IndexedSymbol](LastRow("WS"), FirstRow("AnyComment"), 0)) ++
    rule("S", "WS")
    

  property("comments") = parsesUniquely(g_comments, 
      """/* // 
         */
               
         /* */""", "S")
     

       
}