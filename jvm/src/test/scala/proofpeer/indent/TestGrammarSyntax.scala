package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._

object TestGrammarSyntax extends Properties("GrammarSyntax") {

  import GrammarSyntax.{parseId, parseSymbol, parseSymbols}
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

  property("symbol1") = parseSymbol("x") == Some((x, Nil))
  property("symbol2") = parseSymbol("x ()") == Some((x, Nil))
  property("symbol3") = parseSymbol("x 1") == Some((x, Const(1)))
  property("symbol4") = parseSymbol("x -10") == None
  property("symbol5") = parseSymbol("x (-10)") == Some((x, Neg(Const(10))))
  property("symbol6") = parseSymbol("x (x.leftMostFirst+3)") == 
    Some((x, Add(LayoutEntity(Constraint.LeftMostFirst(x)), Const(3))))
  property("symbol7") = parseSymbol("x (1 | 2 - 5 | 3)") == 
    Some((x, Alternative(Alternative(Const(1), Sub(Const(2), Const(5))), Const(3))))

  property("symbols1") = parseSymbols("x") == Some(Vector((x, Nil)))
  property("symbols2") = parseSymbols("x x") == Some(Vector((x, Nil), (x, Nil)))
  property("symbols3") = parseSymbols("x() x~") == Some(Vector((x, Nil), (x, Current)))
  property("symbols4") = parseSymbols("x 1") == Some(Vector((x, Const(1))))
  property("symbols5") = parseSymbols("x~") == Some(Vector((x, Current)))
  property("symbols6") = parseSymbols("x~x") == Some(Vector((x, Current), (x, Nil)))
  property("symbols7") = parseSymbols("x(-~)") == Some(Vector((x, Neg(Current))))


}