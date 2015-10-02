package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestGrammarSyntax extends Properties("GrammarSyntax") {

  import GrammarSyntax.{parseId, parseParam, parseSymbol, parseSymbols}
  import ParseParam._

  property("id1") = parseId("x") == Some(IndexedSymbol("x", None))
  property("id2") = parseId("x_1") == Some(IndexedSymbol("x", Some("1")))
  property("id3") = parseId("x_123") == Some(IndexedSymbol("x", Some("123")))
  property("id4") = parseId("x_a") == Some(IndexedSymbol("x_a", None))
  property("id5") = parseId("x_a_1") == Some(IndexedSymbol("x_a", Some("1")))
  property("id6") = parseId("x_1_a") == Some(IndexedSymbol("x_1_a", None))
  property("id7") = parseId("1") == None
  property("id8") = parseId("_") == None
  property("id9") = parseId("1a") == None
  property("id10") = parseId("_a") == None
  property("id11") = parseId("") == None

  private val x = IndexedSymbol("x", None)

  private val Nil = Const(NIL)

  property("symbol1") = parseSymbol("x") == Some((x, Nil))
  property("symbol2") = parseSymbol("x ()") == Some((x, Nil))
  property("symbol3") = parseSymbol("x 1") == Some((x, Const(INT(1))))
  property("symbol4") = parseSymbol("x -10") == None
  property("symbol5") = parseSymbol("x (-10)") == Some((x, Neg(Const(INT(10)))))
  property("symbol6") = parseSymbol("x (x.leftMostFirst+3)") == 
    Some((x, Add(LayoutEntity(Constraint.LeftMostFirst(x)), Const(INT(3)))))
  property("symbol7") = parseSymbol("x (1 | 2 - 5 | 3)") == 
    Some((x, Alternative(Alternative(Const(INT(1)), Sub(Const(INT(2)), Const(INT(5)))), Const(INT(3)))))

  property("param1") = parseParam("~.1") == Some(Select(Current, 1))
  property("param2") = parseParam("~.min") == Some(Min(Current))
  property("param3") = parseParam("~.max") == Some(Max(Current))
  property("param4") = parseParam("(~, ~)") == Some(Cons(Current, Cons(Current, Nil)))
  property("param5") = parseParam("()") == Some(Nil)
  property("param6") = parseParam("(~)") == Some(Current)


  property("symbols1") = parseSymbols("x") == Some(Vector((x, Nil)))
  property("symbols2") = parseSymbols("x x") == Some(Vector((x, Nil), (x, Nil)))
  property("symbols3") = parseSymbols("x() x~") == Some(Vector((x, Nil), (x, Current)))
  property("symbols4") = parseSymbols("x 1") == Some(Vector((x, Const(INT(1)))))
  property("symbols5") = parseSymbols("x~") == Some(Vector((x, Current)))
  property("symbols6") = parseSymbols("x~x") == Some(Vector((x, Current), (x, Nil)))
  property("symbols7") = parseSymbols("x(-~)") == Some(Vector((x, Neg(Current))))


}