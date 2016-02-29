package proofpeer.indent

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import proofpeer.indent.regex._
import proofpeer.indent.earley.Earley
import proofpeer.indent.lr1._

object TestErrorRecovery extends Properties("ErrorRecovery") {

  val letter = alt(chars('a', 'z'), chars('A', 'Z'))
  val digit = chars('0', '9')
  val underscore = char('_')
  val id = seq(letter, REPEAT(alt(letter, digit, underscore)))
  val num = REPEAT1(digit)

  val reservedid = alt(string("if"), string("then"), string("else"))

  val idLexer = Lexer.except(Lexer.untilWhitespace(id), Lexer.untilWhitespace(reservedid))

  var _PRIORITY_ : Int = 1

  def lrule(name : String, r : RegularExpr, fallback : Boolean = false) : Grammar = {
    _PRIORITY_ = _PRIORITY_ + 1
    rule(name, r, Some(_PRIORITY_), if (fallback) FALLBACK_SCOPE else "default")
  }

  def lrule(name : String, l : Lexer, fallback : Boolean) : Grammar = {
    _PRIORITY_ = _PRIORITY_ + 1
    lexrule(name, l, Some(_PRIORITY_), if (fallback) FALLBACK_SCOPE else "default")
  }

  def erule(name : String, r : RegularExpr) : Grammar = lrule(name, r, true)

  def erule(name : String, l : Lexer) : Grammar = lrule(name, l, true)

  def grule(name : String, rhs : String) : Grammar = {
    rule(name, rhs, c => name)
  }

  def grule(name : String, rhs : String, action : ParseContext => Any) : Grammar = {
    rule(name, rhs, action)
  }


  sealed trait Value
  final case object Undefined extends Value {
    override def toString : String = "?"
  }
  final case class Num(x : BigInt) extends Value {
    override def toString : String = x.toString
  }
  final case class Func(env : Map[String, Value], name : String, body : ParseTree) extends Value {
    override def toString : String = "λ"
  }

  def ibool(b : Boolean) : Value = if (b) Num(1) else Num(0)

  val grammar1 =
    lrule("id", idLexer, false) ++
    lrule("num", num) ++
    lrule("if", string("if")) ++
    lrule("then", string("then")) ++
    lrule("else", string("else")) ++
    lrule("lambda", char('λ')) ++
    lrule("plus", char('+')) ++
    lrule("minus", char('-')) ++
    lrule("div", char('/')) ++
    lrule("mul", char('*')) ++
    lrule("semicolon", char(';')) ++
    lrule("dot", char('.')) ++
    lrule("eq", char('=')) ++
    lrule("less", char('<')) ++
    lrule("leq", string("<=")) ++
    lrule("greater", char('>')) ++
    lrule("geq", string(">=")) ++
    lrule("assign", char('=')) ++
    lrule("open", char('(')) ++
    lrule("close", char(')')) ++
    lrule("undefined", char('?')) ++
    grule("S", "P") ++
    grule("P", "E") ++
    grule("P", "L E") ++
    grule("L", "A") ++
    grule("L", "L A") ++
    grule("A", "id assign E semicolon") ++
    grule("E", "IF") ++
    grule("E", "LAMBDA") ++
    grule("E", "CMP") ++
    grule("IF", "if E then P_1 else P_2") ++
    grule("CMP", "ARITH") ++
    grule("CMP", "ARITH COP ARITH", _.COP[Any]) ++
    grule("COP", "eq", c => (x : BigInt, y : BigInt) => ibool(x == y)) ++
    grule("COP", "less", c => (x : BigInt, y : BigInt) => ibool(x < y)) ++
    grule("COP", "leq", c => (x : BigInt, y : BigInt) => ibool(x <= y)) ++
    grule("COP", "greater", c => (x : BigInt, y : BigInt) => ibool(x > y)) ++
    grule("COP", "geq", c => (x : BigInt, y : BigInt) => ibool(x >= y)) ++
    grule("ARITH", "ADD") ++
    grule("ADD", "ADD plus MUL", c => (x : BigInt, y : BigInt) => Num(x + y)) ++
    grule("ADD", "ADD minus MUL", c => (x : BigInt, y : BigInt) => Num(x - y)) ++
    grule("ADD", "MUL") ++
    grule("ADD", "minus MUL") ++
    grule("MUL", "MUL mul FACTOR", c => (x : BigInt, y : BigInt) => Num(x * y)) ++
    grule("MUL", "MUL div FACTOR", c => (x : BigInt, y : BigInt) => if (y == 0) Undefined else Num(x / y)) ++
    grule("MUL", "FACTOR") ++
    grule("FACTOR", "APP") ++
    grule("APP", "ATOMIC") ++
    grule("APP", "APP ATOMIC") ++
    grule("ATOMIC", "id") ++
    grule("ATOMIC", "num") ++
    grule("ATOMIC", "undefined") ++
    grule("ATOMIC", "open P close") ++
    grule("LAMBDA", "lambda id dot E")

  def range(cs : Char*) : Range = {
    var r = Range()
    for (c <- cs) r = r + Range(c)
    r
  }

  final case class Until(expr : RegularExpr, exclusive : Boolean) 

  object Until {

    def Incl(c : Int) : Until = Until(char(c), false)

    def Incl(s : String) : Until = Until(string(s), false)

    def Excl(c : Int) : Until = Until(char(c), true)

    def Excl(s : String) : Until = Until(string(s), true)

  }

  // Searches for incl, and no excls make appear in between
  def search(bail : Boolean, emptyIfFailure : Boolean, incl : RegularExpr, excls : RegularExpr*) : Lexer = {

    val inclLexer : Lexer = Lexer.untilWhitespace(incl)
    val exclLexers : Seq[Lexer] = excls.map(u => Lexer.untilWhitespace(u))

    new Lexer {
      
      def lex(d : Document, startPosition : Int, param : ParseParam.V) : (Int, ParseParam.V) = {
        val size = d.size
        var pos = startPosition
        while (pos < size) {
          val (len, _) = inclLexer.lex(d, pos, ParseParam.NIL)
          if (len >= 0) return (pos + len - startPosition, ParseParam.UNDEFINED)
          for (lexer <- exclLexers) {
            val (len, _) = lexer.lex(d, pos, ParseParam.NIL)
            if (len >= 0) {
              if (bail) return (pos - startPosition, ParseParam.UNDEFINED)
              else return (if (emptyIfFailure) 0 else -1, ParseParam.UNDEFINED)
            }
          }
          pos += 1
        }
        if (bail) return (size - startPosition, ParseParam.UNDEFINED)
        else return (if (emptyIfFailure) 0 else -1, ParseParam.UNDEFINED)
      }

      def zero = inclLexer.zero || bail

      def first = Range.universal   
    }
  
  }
  
  def find(emptyIfFailure : Boolean, incl : String, excls : String*) : Lexer = {
    search(false, emptyIfFailure, string(incl), excls.map(e => string(e)) : _*)
  }

  def bail(incl : String, excls : String*) : Lexer = {
    search(true, true, string(incl), excls.map(e => string(e)) : _*)
  }

  import Until._

  val grammar2 = grammar1 ++
    erule("trailing", Lexer.untilEnd(REPEAT1(CHAR(Range.universal)))) ++
    erule("blank", EMPTY) ++
    erule("err_close", find(true, ")")) ++
    erule("err_dot", find(true, ".", ";", ")", "λ")) ++ 
    erule("err_then", find(true, "then")) ++
    erule("err_else", find(true, "else")) ++
    erule("err_optsemicolon", find(true, ";")) ++
    erule("err_semicolon", find(false, ";")) ++
    erule("err_assign", find(true, "=")) ++
    grule("S", "P trailing") ++
    grule("FACTOR", "blank") ++
    grule("ATOMIC", "open P err_close") ++
    grule("LAMBDA", "lambda err_dot E") ++
    grule("LAMBDA", "lambda id err_dot E") ++
    grule("IF", "if E err_then P_1 err_else P_2") ++
    grule("IF", "if E then P_1 err_else P_2") ++
    grule("A", "err_semicolon") ++
    grule("A", "id err_assign E err_optsemicolon") ++
    grule("A", "id assign E err_optsemicolon")

/*
err_close : go until you find a closing bracket, but don't jump across semicolons
(2 + 3;4+(34))
x = (5+2;
y = 13;
y * 15
y / 

2 * if 3 then 5 else 10

*/


  //val parser1 = Parser(grammar1)
  val lr1 = new LR1Parser(grammar1, "S")
  val lr2 = new LR1Parser(grammar2, "S")

  def tree2str(tree : ParseTree) : String = {
    tree match {
      case tree : NonterminalNode if tree.rhs.size > 1 =>
        var s : String = "(" + tree.symbol
        for (t <- tree.rhs) s = s + " " + tree2str(t)
        s + ")"
      case tree : NonterminalNode if tree.rhs.size == 1 =>
        tree2str(tree.rhs.head)
      case _ => tree.symbol
    }
  }

  class Eval(grammar : Grammar, doc : Document) {

    def text(tree : ParseTree) : String = doc.getText(tree.span)

    def treeOf(tree : ParseTree, id : IndexedSymbol) : ParseTree = {
      tree match {
        case tree: NonterminalNode =>
          val index = grammar.parserules(tree.symbol)(tree.ruleindex).rhs.indexOf(id)
          if (index < 0) throw new RuntimeException("cannot lookup symbol " + id)
          tree.rhs(index)
        case _ =>
          throw new RuntimeException("invalid argument to treeOf")
      }
    }

    def defined(tree : ParseTree, ids : IndexedSymbol*) : Boolean = {
      tree match {
        case tree: NonterminalNode =>
          for (id <- ids)
            if (grammar.parserules(tree.symbol)(tree.ruleindex).rhs.indexOf(id) < 0) return false
          true
        case _ =>
          throw new RuntimeException("invalid argument to defined")
      }      
    }

    def eval(env : Map[String, Value], tree : ParseTree) : Value = {
      tree match {
        case tree : TerminalNode =>
          tree.symbol match {
            case "id" => 
              env.get(text(tree)) match {
                case None => Undefined
                case Some(v) => v
              }
            case "num" =>
              Num(text(tree).toInt)
            case "undefined" =>
              Undefined
            case _ => Undefined
          }
        case tree : NonterminalNode if tree.rhs.size == 1 =>
          eval(env, tree.rhs(0))
        case tree : NonterminalNode =>
          tree.symbol match {
            case "LAMBDA" => 
              if (defined(tree, "id", "E")) {
                val id = text(treeOf(tree, "id"))
                val body = treeOf(tree, "E")
                Func(env, id, body)
              } else if (defined(tree, "E")) {
                val body = treeOf(tree, "E")
                Func(env, "?", body)
              } else
                Undefined
            case "ATOMIC" =>
              eval(env, treeOf(tree, "P"))
            case x if tree.rhs.size == 3 && (x == "CMP" || x == "ADD" || x == "MUL") =>
              (eval(env, tree.rhs(0)), eval(env, tree.rhs(2)), tree.getValue[Any]) match {
                case (Num(x), Num(y), f : Function2[_, _, _]) => f.asInstanceOf[Function2[BigInt, BigInt, Value]](x, y)
                case _ => Undefined
              }
            case "ADD" => // ADD => minus MUL
              eval(env, treeOf(tree, "MUL")) match {
                case Num(x) => Num(-x)
                case _ => Undefined
              }
            case "APP" =>
              (eval(env, treeOf(tree, "APP")), eval(env, treeOf(tree, "ATOMIC"))) match {
                case (Func(env, x, body), v : Value) =>
                  eval(env + (x -> v), body)
                case _ => Undefined 
              }
            case "IF" => 
              eval(env, treeOf(tree, "E")) match {
                case Num(x) if x != 0 =>
                  eval(env, treeOf(tree, "P_1"))
                case Num(x) if x == 0 =>
                  eval(env, treeOf(tree, "P_2"))
                case _ => Undefined
              }
            case "P" => 
              val env1 = evalEnv(env, treeOf(tree, "L"))
              eval(env1, treeOf(tree, "E"))
            case "S" =>
              eval(env, treeOf(tree, "P"))
            case _ => throw new RuntimeException("cannot eval: " + tree)
          }
        case _ => throw new RuntimeException("cannot eval: " + tree)
      }
    }

    def evalEnv(env : Map[String, Value], _tree : ParseTree) : Map[String, Value] = {
      val tree = _tree.asInstanceOf[NonterminalNode]
      tree.symbol match {
        case "A" if defined(tree, "id", "E") => 
          val id = text(treeOf(tree, "id"))
          val v = eval(env, treeOf(tree, "E"))
          env + (id -> v)
        case "A" =>
          env
        case "L" if defined(tree, "L", "A") =>
          val env1 = evalEnv(env, treeOf(tree, "L"))
          evalEnv(env1, treeOf(tree, "A"))
        case "L" => 
          evalEnv(env, treeOf(tree, "A"))
        case _ =>
          throw new RuntimeException("cannot evalEnv: " + tree)
      }
    }

  }

  def ok(s : String) : Boolean = {
    lr1.parseAsTree(s) match {
      case None => 
        println("cannot parse: '" + s + "'")
        false
      case Some((doc,tree)) =>
        println("parsed '" + s + "' successfully: " + tree2str(tree))
        true
    }
  }

  def eok(s : String) : Boolean = {
    lr2.parseAsTree(s) match {
      case None => 
        println("cannot parse: '" + s + "'")
        false
      case Some((doc,tree)) =>
        println("parsed '" + s + "' successfully: " + tree2str(tree))
        true
    }
  }  

  def exec(s : String, result : String) : Boolean = {
    lr2.parseAsTree(s) match {
      case None => 
        println("cannot parse: '" + s + "'")
        println("")
        false
      case Some((doc, tree)) =>
        println("parsed successfully: '" + s + "'")
        println("  tree = " + tree2str(tree))
        val E = new Eval(grammar2, doc)
        val v = E.eval(Map(), tree)
        println("  eval = " + v)
        println("")
        v.toString == result
    }
  }

  def cope(s : String, result : String = "?") : Boolean = {
    !ok(s) && exec(s, result)
  }

  property("1") = ok("a") && ok("1") && ok("1 + a") && ok("-1") && ok("- 3 * 8 + 5") && ok("-(-3)")
  property("2") = ok("a < b + 1") && ok("x = 10; x * x") && ok("x = 10; (x) = 10")
  property("3") = ok("if x > 0 then f 5 x + 1 else λ y. y-x")

  property("x1") = exec("2", "2") && exec("a", "?") && exec("?", "?") && exec("λ x. 2 * x", "λ") && exec("((4))", "4")
  property("x2") = exec("2 + 3", "5") && exec("2 / 0", "?") && exec("9 / 2", "4") && exec("9 - 12", "-3") && exec("4 * 5", "20") 
  property("x3") = exec("-10", "-10") && exec("(λ x. λ y. x * x + y) 12 7", "151") 
  property("if") = exec("if 1 > 0 then 5 else ?", "5") && exec("if 1 < 0 then ? else 7", "7")
  property("P") = exec("sqr = λ x. x * x; y = 12; sqr y", "144")
  property("factorial") =
    exec("Z = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v))); Fac = λf. λn. if n <= 0 then 1 else n * f(n - 1); fac = Z Fac; fac 10", "3628800")

  property("e1") = cope(")")
  property("e2") = cope("(")
  property("e3") = cope("+")
  property("e4") = cope("5+")
  property("e5") = cope("5+)")
  property("e6") = cope("(5+")
  property("e7") = cope("(5+)")
  property("e8") = cope("((5", "5")
  property("e9") = cope(".")
  property("e10") = cope("(λ $. 10) x", "10")
  property("e11") = cope("(λ $. 10 $) x", "10")
  property("e12") = cope("λ $. 10 $", "λ")
  property("e13") = cope("(λ x) 3")
  property("e14") = cope("if 1 else 10")
  property("e15") = exec("if 3 * else then 10 else 20", "?")
  property("e16") = cope("if (4 then 5 else 10", "5")
  property("e17") = cope("if 4) then 5 else 10", "5")
  property("e18") = cope("x = 5; $; x", "5")
  property("e19") = cope("(x = 5; $; x)", "5")
  property("e20") = cope("(x = 5; y $; x)")
  property("e21") = cope("(x = 5; y * $; x)")
  property("e22") = cope("x = 5; y $; x")
  property("e23") = cope("2 * if 3 then 5 else 10")

}