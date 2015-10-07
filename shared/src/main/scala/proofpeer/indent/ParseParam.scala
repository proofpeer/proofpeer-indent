package proofpeer.indent

sealed trait ParseParam 

final object ParseParam {

  sealed trait V
  final case object NIL extends V {
    override def toString = "nil"
  }
  final case class INT(x : Int) extends V {
    override def toString = x.toString
  }
  final case class LIST(head : V, tail : V) extends V {
    override def toString : String = {
      var s = "[" + head
      var l = tail
      while(true) {
        l match {
          case NIL => return s + "]"
          case LIST(head, tail) => 
            s = s + "," + head
            l = tail
          case INT(x) => return s + "/" + x + "]"
        }
      }
      throw new RuntimeException("internal error")
    }
  }

  final case class Const(v : V) extends ParseParam
  final case object Current extends ParseParam
  final case class LayoutEntity(e : Constraint.LayoutEntity) extends ParseParam
  final case class Cons(head : ParseParam, tail : ParseParam) extends ParseParam
  final case class Add(p : ParseParam, q : ParseParam) extends ParseParam
  final case class Sub(p : ParseParam, q : ParseParam) extends ParseParam
  final case class Neg(p : ParseParam) extends ParseParam
  final case class Min(p : ParseParam) extends ParseParam
  final case class Max(p : ParseParam) extends ParseParam
  final case class Select(p : ParseParam, index : Int) extends ParseParam
  final case class Alternative(preferred : ParseParam, alternative : ParseParam) extends ParseParam

  def noParams(l : Int) : Vector[ParseParam] = {
    Vector.fill(l)(ParseParam.Const(ParseParam.NIL))
  }

  def collectSymbols(param : ParseParam) : Set[IndexedSymbol] = {
    param match {
      case _ : Const => Set()
      case Current => Set()
      case LayoutEntity(e) => Set(e.s)
      case Cons(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case Add(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case Sub(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case Neg(p) => collectSymbols(p)     
      case Min(p) => collectSymbols(p)
      case Max(p) => collectSymbols(p)
      case Select(p, _) => collectSymbols(p)
      case Alternative(p, q) => collectSymbols(p) ++ collectSymbols(q)
    }
  }

  def collectSymbols(params : Vector[ParseParam]) : Set[IndexedSymbol] = {
    var symbols : Set[IndexedSymbol] = Set()
    for (p <- params) symbols = symbols ++ collectSymbols(p)
    symbols
  }

  def evalParams(params : Vector[ParseParam], f : IndexedSymbol => Int) : (V, Span.Layout, Int) => V = {
    val evals = params.map(p => evalParam(simp(p), f))
    (param : V, layout : Span.Layout, i : Int) => evals(i)(param, layout)
  }

  private def error[T](s : String) : T =
    throw new RuntimeException(s)

  private def calcAdd(p : V, q : V) : V = {
    (p, q) match {
      case (INT(x), INT(y)) => INT(x + y)
      case _ => error ("calcAdd " + p + " " + q)
    }  
  }

  private def calcSub(p : V, q : V) : V = {
    (p, q) match {
      case (INT(x), INT(y)) => INT(x - y)
      case _ => error ("calcSub " + p + " " + q)
    }  
  }

  private def calcNeg(p : V) : V = {
    p match {
      case INT(x) => INT(-x)
      case _ => error ("calcNeg " + p)
    }
  }

  private def calcMax(p : V) : V = {
    p match {
      case NIL => p
      case _ : INT => p
      case LIST(NIL, q) => calcMax(q)
      case LIST(p @ INT(x), q) =>
        calcMax(q) match {
          case NIL => p
          case q @ INT(y) => if (x >= y) p else q
          case q => error("calcMax " + p + " " + q)
        }
      case _ => error("calcMax " + p)
    }
  }

  private def calcMin(p : V) : V = {
    p match {
      case NIL => p
      case _ : INT => p
      case LIST(NIL, q) => calcMin(q)
      case LIST(p @ INT(x), q) =>
        calcMin(q) match {
          case NIL => p
          case q @ INT(y) => if (x <= y) p else q
          case q => error("calcMin " + p + " " + q)
        }
      case _ => error("calcMin " + p)
    }
  }

  def calcSelect(p : V, index : Int) : V = {
    p match {
      case LIST(p, q) if index == 1 => p
      case LIST(p, q) if index > 1 => calcSelect(q, index-1)
      case _ => error("calcSelect " + p + " " + index)
    }
  }

  def toInt(p : V) : Int = {
    p match {
      case ParseParam.INT(p) => p
      case _ => throw new RuntimeException("INT parameter expected, found: " + p)
    } 
  }
    
  private def isConst(p : ParseParam) : Boolean = {
    p match {
      case _ : Const => true
      case _ => false
    }
  }
  
  private def destConst(p : ParseParam) : V = {
    p match {
      case Const(v) => v
      case _ => throw new RuntimeException("cannot destConst " + p)
    }
  }

  private def evalParam(param : ParseParam, f : IndexedSymbol => Int) : (V, Span.Layout) => V = {
    param match {
      case Const(NIL) => (p : V, layout : Span.Layout) => NIL
      case Const(c) => (p : V, layout : Span.Layout) => c
      case Current => (p : V, layout : Span.Layout) => p
      case LayoutEntity(e) => 
        val i = f(e.s)
        val q = e.q
        (p : V, layout : Span.Layout) => {
          q.get(layout(i)) match {
            case None => NIL
            case Some(x) => if (x < 0) NIL else INT(x)
          } 
        }
      case Cons(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout) => LIST(U(p, layout), V(p, layout))
      case Add(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout) => calcAdd(U(p, layout), V(p, layout))
      case Sub(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout) => calcSub(U(p, layout), V(p, layout))
      case Neg(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout) => calcNeg(U(p, layout))   
      case Max(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout) => calcMax(U(p, layout))   
      case Min(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout) => calcMin(U(p, layout))   
      case Select(u, i) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout) => calcSelect(U(p, layout), i)  
      case Alternative(preferred, alternative) =>
        val ep = evalParam(preferred, f)
        val ea = evalParam(alternative, f)
        (p : V, layout : Span.Layout) => {
          val u = ep(p, layout)
          if (u == NIL) ea(p, layout) else u
        }
    }
  }

  private def simp(p : ParseParam) : ParseParam = {
    p match {
      case Current => p
      case _ : Const => p
      case _ : LayoutEntity => p
      case Neg(p) => 
        simp(p) match {
          case Const(c) => Const(calcNeg(c))
          case p => Neg(p)
        }
      case Add(p, q) =>
        (simp(p), simp(q)) match {
          case (Const(u), Const(v)) => Const(calcAdd(u, v))
          case (p, q) => Add(p, q)
        }
      case Sub(p, q) =>
        (simp(p), simp(q)) match {
          case (Const(u), Const(v)) => Const (calcSub(u, v))
          case (p, q) => Sub(p, q)
        }
      case Max(p) => 
        simp(p) match {
          case Const(u) => Const(calcMax(u))
          case u => u
        }
      case Min(p) => 
        simp(p) match {
          case Const(u) => Const(calcMin(u))
          case u => u
        }
      case Select(p, index) => 
        simp(p) match {
          case Const(u) => Const(calcSelect(u, index))
          case u => Select(u, index)
        }
      case Alternative(p, q) =>
        (simp(p), simp(q)) match {
          case (Const(NIL), q) => q
          case (p, q) => Alternative(p, q)
        }
      case Cons(p, q) => 
        (simp(p), simp(q)) match {
          case (Const(p), Const(q)) => Const(LIST(p, q))
          case (p, q) => Cons(p, q)
        }
    }
  }

  def v2Ints(defaults : List[Int], v : V) : List[Int] = {
    if (defaults.isEmpty) List()
    else {
      v match {
        case LIST(INT(x), tail) => x :: v2Ints(defaults.tail, tail)
        case LIST(NIL, tail) => defaults.head :: v2Ints(defaults.tail, tail)
        case INT(x) => x :: defaults.tail
        case _ => defaults
      }
    }
  }

}


