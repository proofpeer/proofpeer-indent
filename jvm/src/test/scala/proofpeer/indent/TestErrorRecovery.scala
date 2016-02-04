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

  var _PRIORITY_ : Int = 1

  def lexrule(name : String, r : RegularExpr, fallback : Boolean = false) : Grammar = {
    _PRIORITY_ = _PRIORITY_ + 1
    rule(name, r, Some(_PRIORITY_), if (fallback) FALLBACK_SCOPE else "default")
  }

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
    lexrule("id", id) ++
    lexrule("num", num) ++
    lexrule("if", string("if")) ++
    lexrule("then", string("then")) ++
    lexrule("else", string("else")) ++
    lexrule("lambda", char('λ')) ++
    lexrule("plus", char('+')) ++
    lexrule("minus", char('-')) ++
    lexrule("div", char('/')) ++
    lexrule("mul", char('*')) ++
    lexrule("semicolon", char(';')) ++
    lexrule("dot", char('.')) ++
    lexrule("eq", char('=')) ++
    lexrule("less", char('<')) ++
    lexrule("leq", string("<=")) ++
    lexrule("greater", char('>')) ++
    lexrule("geq", string(">=")) ++
    lexrule("assign", char('=')) ++
    lexrule("open", char('(')) ++
    lexrule("close", char(')')) ++
    lexrule("undefined", char('?')) ++
    grule("P", "E") ++
    grule("P", "L semicolon E") ++
    grule("L", "A") ++
    grule("L", "L semicolon A") ++
    grule("A", "id assign E") ++
    grule("E", "IF") ++
    grule("E", "LAMBDA") ++
    grule("E", "CMP") ++
    grule("IF", "if E_1 then E_2 else E_3") ++
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
    grule("MUL", "MUL mul APP", c => (x : BigInt, y : BigInt) => Num(x * y)) ++
    grule("MUL", "MUL div APP", c => (x : BigInt, y : BigInt) => if (y == 0) Undefined else Num(x / y)) ++
    grule("MUL", "APP") ++
    grule("APP", "ATOMIC") ++
    grule("APP", "APP ATOMIC") ++
    grule("ATOMIC", "id") ++
    grule("ATOMIC", "num") ++
    grule("ATOMIC", "undefined") ++
    grule("ATOMIC", "open P close") ++
    grule("LAMBDA", "lambda id dot E")

  val parser1 = Parser(grammar1)
  val lr1 = new LR1Parser(grammar1, "P")

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
            case _ => throw new RuntimeException("cannot eval: " + tree)
          }
        case tree : NonterminalNode if tree.rhs.size == 1 =>
          eval(env, tree.rhs(0))
        case tree : NonterminalNode =>
          tree.symbol match {
            case "LAMBDA" => 
              val id = text(treeOf(tree, "id"))
              val body = treeOf(tree, "E")
              Func(env, id, body)
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
              eval(env, treeOf(tree, "E_1")) match {
                case Num(x) if x != 0 =>
                  eval(env, treeOf(tree, "E_2"))
                case Num(x) if x == 0 =>
                  eval(env, treeOf(tree, "E_3"))
                case _ => Undefined
              }
            case "P" => 
              val env1 = evalEnv(env, treeOf(tree, "L"))
              eval(env1, treeOf(tree, "E"))
            case _ => throw new RuntimeException("cannot eval: " + tree)
          }
        case _ => throw new RuntimeException("cannot eval: " + tree)
      }
    }

    def evalEnv(env : Map[String, Value], _tree : ParseTree) : Map[String, Value] = {
      val tree = _tree.asInstanceOf[NonterminalNode]
      (tree.symbol, tree.rhs.size) match {
        case ("A", 3) => 
          val id = text(treeOf(tree, "id"))
          val v = eval(env, treeOf(tree, "E"))
          env + (id -> v)
        case ("L", 1) => 
          evalEnv(env, treeOf(tree, "A"))
        case ("L", 3) =>
          val env1 = evalEnv(env, treeOf(tree, "L"))
          evalEnv(env1, treeOf(tree, "A"))
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

  def exec(s : String, result : String) : Boolean = {
    lr1.parseAsTree(s) match {
      case None => 
        println("cannot parse: '" + s + "'")
        println("")
        false
      case Some((doc, tree)) =>
        print("parsed successfully: '" + s + "', eval = ")
        val E = new Eval(grammar1, doc)
        val v = E.eval(Map(), tree)
        println(v)
        println("")
        v.toString == result
    }
  }

  property("grammar1_1") = ok("a") && ok("1") && ok("1 + a") && ok("-1") && ok("- 3 * 8 + 5") && ok("-(-3)")
  property("grammar1_2") = ok("a < b + 1") && ok("x = 10; x * x") && ok("x = 10; (x) = 10")
  property("grammar1_3") = ok("if x > 0 then f 5 x + 1 else λ y. y-x")

  property("grammar1_x1") = exec("2", "2") && exec("a", "?") && exec("?", "?") && exec("λ x. 2 * x", "λ") && exec("((4))", "4")
  property("grammar1_x2") = exec("2 + 3", "5") && exec("2 / 0", "?") && exec("9 / 2", "4") && exec("9 - 12", "-3") && exec("4 * 5", "20") 
  property("grammar1_x3") = exec("-10", "-10") && exec("(λ x. λ y. x * x + y) 12 7", "151") 
  property("grammar1_if") = exec("if 1 > 0 then 5 else ?", "5") && exec("if 1 < 0 then ? else 7", "7")
  property("grammar1_P") = exec("sqr = λ x. x * x; y = 12; sqr y", "144")
  property("grammar1_factorial") =
    exec("Z = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v))); Fac = λf. λn. if n <= 0 then 1 else n * f(n - 1); fac = Z Fac; fac 10", "3628800")



}