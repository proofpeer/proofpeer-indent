package proofpeer.indent

sealed trait ParseParam 

final object ParseParam {

  sealed trait V
  final case object UNDEFINED extends V
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
          case UNDEFINED => return s + "/undefined]"
          case LIST(head, tail) => 
            s = s + "," + head
            l = tail
          case INT(x) => return s + "/" + x + "]"
        }
      }
      throw new RuntimeException("internal error")
    }
  }

  type Results = Vector[V]

  def resultsAreEqual(u : Results, v : Results) : Boolean = {
    u == v
  }

  private def completeResults(results : Results, param : V, layout : Span.Layout, coreItem : earley.CoreItem) : Results = 
  {
    if (results.size < coreItem.rhs.size) results
    else {
      val r = coreItem.evalResult(param, layout, results)
      val results1 = results :+ r
      if (results1.size != layout.size || layout.size != coreItem.rhs.size + 1)
        throw new RuntimeException("internal error: failed the sanity check in completeResults")
      results1
    }
  }

  def emptyResults(param : V, layout : Span.Layout, coreItem : earley.CoreItem) : Results = 
    completeResults(Vector[V](), param, layout, coreItem)

  def addToResults(results : Results, result : V, param : V, 
    layout : Span.Layout, coreItem : earley.CoreItem) : Results = 
  {   
    completeResults(results :+ result, param, layout, coreItem)
  }

  def getResult(results : Results) : V = results(results.size - 1)

  final case class Const(v : V) extends ParseParam
  final case object Current extends ParseParam
  final case class LayoutEntity(e : Constraint.LayoutEntity) extends ParseParam
  final case class VResult(s : IndexedSymbol) extends ParseParam
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

  def collectLayoutSymbols(param : ParseParam) : Set[IndexedSymbol] = {
    param match {
      case _ : Const => Set()
      case Current => Set()
      case LayoutEntity(e) => Set(e.s)
      case VResult(s) => Set()
      case Cons(p, q) => collectLayoutSymbols(p) ++ collectLayoutSymbols(q)
      case Add(p, q) => collectLayoutSymbols(p) ++ collectLayoutSymbols(q)
      case Sub(p, q) => collectLayoutSymbols(p) ++ collectLayoutSymbols(q)
      case Neg(p) => collectLayoutSymbols(p)     
      case Min(p) => collectLayoutSymbols(p)
      case Max(p) => collectLayoutSymbols(p)
      case Select(p, _) => collectLayoutSymbols(p)
      case Alternative(p, q) => collectLayoutSymbols(p) ++ collectLayoutSymbols(q)
    }
  }

  def collectResultSymbols(param : ParseParam) : Set[IndexedSymbol] = {
    param match {
      case _ : Const => Set()
      case Current => Set()
      case LayoutEntity(e) => Set()
      case VResult(s) => Set(s)
      case Cons(p, q) => collectResultSymbols(p) ++ collectResultSymbols(q)
      case Add(p, q) => collectResultSymbols(p) ++ collectResultSymbols(q)
      case Sub(p, q) => collectResultSymbols(p) ++ collectResultSymbols(q)
      case Neg(p) => collectResultSymbols(p)     
      case Min(p) => collectResultSymbols(p)
      case Max(p) => collectResultSymbols(p)
      case Select(p, _) => collectResultSymbols(p)
      case Alternative(p, q) => collectResultSymbols(p) ++ collectResultSymbols(q)
    }
  }

  def collectAllSymbols(param : ParseParam) : Set[IndexedSymbol] =
    collectLayoutSymbols(param) ++ collectResultSymbols(param)

  def collectAllSymbols(params : Vector[ParseParam]) : Set[IndexedSymbol] = {
    var symbols : Set[IndexedSymbol] = Set()
    for (p <- params) symbols = symbols ++ collectAllSymbols(p)
    symbols
  }

  def evaluateParams(params : Vector[ParseParam], f : IndexedSymbol => Int) : 
    (V, Span.Layout, Results, Int) => V = 
  {
    val evals = params.map(p => evaluateParam(p, f))
    (param : V, layout : Span.Layout, results : Results, i : Int) => evals(i)(param, layout, results)
  }

  private def calcAdd(p : V, q : V) : V = {
    (p, q) match {
      case (INT(x), INT(y)) => INT(x + y)
      case (UNDEFINED, _) => UNDEFINED
      case (_, UNDEFINED) => UNDEFINED
      case _ => NIL
    }  
  }

  private def calcSub(p : V, q : V) : V = {
    (p, q) match {
      case (INT(x), INT(y)) => INT(x - y)
      case (UNDEFINED, _) => UNDEFINED
      case (_, UNDEFINED) => UNDEFINED
      case _ => NIL
    }  
  }

  private def calcNeg(p : V) : V = {
    p match {
      case INT(x) => INT(-x)
      case UNDEFINED => UNDEFINED
      case _ => NIL
    }
  }

  private def calcMax(p : V) : V = {
    p match {
      case NIL => p
      case UNDEFINED => UNDEFINED
      case _ : INT => p
      case LIST(head, tail) => 
        (calcMax(head), calcMax(tail)) match {
          case (p @ INT(x), q @ INT(y)) => if (x >= y) p else q
          case (p @ INT(_), _) => p
          case (_, q @ INT(_)) => q
          case (UNDEFINED, _) => UNDEFINED
          case (_, UNDEFINED) => UNDEFINED
          case (_, _) => NIL
        }
    }
  }

  private def calcMin(p : V) : V = {
    p match {
      case NIL => p
      case UNDEFINED => UNDEFINED
      case _ : INT => p
      case LIST(head, tail) => 
        (calcMin(head), calcMin(tail)) match {
          case (p @ INT(x), q @ INT(y)) => if (x <= y) p else q
          case (p @ INT(_), _) => p
          case (_, q @ INT(_)) => q
          case (UNDEFINED, _) => UNDEFINED
          case (_, UNDEFINED) => UNDEFINED
          case (_, _) => NIL
        }
    }
  }

  private def chooseAlternative(p : V) : Boolean = {
    p match {
      case NIL => true
      case INT(x) => x < 0
      case _ => false
    }
  }

  def calcSelect(p : V, index : Int) : V = {
    p match {
      case UNDEFINED => UNDEFINED
      case LIST(p, q) if index == 1 => p
      case LIST(p, q) if index > 1 => calcSelect(q, index-1)
      case x : INT if index == 1 => x
      case _ => NIL
    }
  }
    
  def toOptionInt(p : V) : Option[Int] = {
    p match {
      case ParseParam.INT(p) => Some(p)
      case _ => None
    } 
  }

  private def isConst(p : ParseParam) : Boolean = {
    p match {
      case _ : Const => true
      case _ => false
    }
  }
  
  def evaluateParam(param : ParseParam, f : IndexedSymbol => Int) :
    (V, Span.Layout, Results) => V = 
  {
    evalParam(simp(param), f)
  }  

  private def evalParam(param : ParseParam, f : IndexedSymbol => Int) : 
    (V, Span.Layout, Results) => V = 
  {
    param match {
      case Const(NIL) => (p : V, layout : Span.Layout, results : Results) => NIL
      case Const(c) => (p : V, layout : Span.Layout, results : Results) => c
      case Current => (p : V, layout : Span.Layout, results : Results) => p
      case LayoutEntity(e) => 
        val i = f(e.s)
        val q = e.q
        (p : V, layout : Span.Layout, results : Results) => {
          q.get(layout(i)) match {
            case None => NIL
            case Some(x) => if (x < 0) NIL else INT(x)
          } 
        }
      case VResult(s) =>
        val i = f(s)
        (p : V, layout : Span.Layout, results : Results) => results(i)
      case Cons(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout, results : Results) => 
          LIST(U(p, layout, results), V(p, layout, results))
      case Add(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout, results : Results) => 
          calcAdd(U(p, layout, results), V(p, layout, results))
      case Sub(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : V, layout : Span.Layout, results : Results) => 
          calcSub(U(p, layout, results), V(p, layout, results))
      case Neg(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout, results : Results) => calcNeg(U(p, layout, results))   
      case Max(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout, results : Results) => calcMax(U(p, layout, results))   
      case Min(u) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout, results : Results) => calcMin(U(p, layout, results))   
      case Select(u, i) =>
        val U = evalParam(u, f)
        (p : V, layout : Span.Layout, results : Results) => calcSelect(U(p, layout, results), i)  
      case Alternative(preferred, alternative) =>
        val ep = evalParam(preferred, f)
        val ea = evalParam(alternative, f)
        (p : V, layout : Span.Layout, results : Results) => {
          val u = ep(p, layout, results)
          if (chooseAlternative(u)) ea(p, layout, results) else u
        }
    }
  }

  private def simp(p : ParseParam) : ParseParam = {
    p match {
      case Current => p
      case _ : Const => p
      case _ : LayoutEntity => p
      case _ : VResult => p
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
          case (Const(c), q) if chooseAlternative(c) => q
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
        case LIST(UNDEFINED, tail) => defaults.head :: v2Ints(defaults.tail, tail)        
        case INT(x) => x :: defaults.tail
        case _ => defaults
      }
    }
  }

}


