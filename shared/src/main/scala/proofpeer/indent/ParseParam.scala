package proofpeer.indent

sealed trait ParseParam 

final object ParseParam {

  final case object Nil extends ParseParam
  final case class Const(c : Int) extends ParseParam
  final case object Current extends ParseParam
  final case class LayoutEntity(e : Constraint.LayoutEntity) extends ParseParam
  final case class Add(p : ParseParam, q : ParseParam) extends ParseParam
  final case class Sub(p : ParseParam, q : ParseParam) extends ParseParam
  final case class Neg(p : ParseParam) extends ParseParam
  final case class Alternative(preferred : ParseParam, alternative : ParseParam) extends ParseParam

  val NIL = -1

  def noParams(l : Int) : Vector[ParseParam] = {
    Vector.fill(l)(Nil)
  }

  def collectSymbols(param : ParseParam) : Set[IndexedSymbol] = {
    param match {
      case LayoutEntity(e) => Set(e.s)
      case Add(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case Sub(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case Neg(p) => collectSymbols(p)     
      case Alternative(p, q) => collectSymbols(p) ++ collectSymbols(q)
      case _ : Const => Set()
      case Nil => Set()
      case Current => Set()
    }
  }

  def collectSymbols(params : Vector[ParseParam]) : Set[IndexedSymbol] = {
    var symbols : Set[IndexedSymbol] = Set()
    for (p <- params) symbols = symbols ++ collectSymbols(p)
    symbols
  }


  def evalParams(params : Vector[ParseParam], f : IndexedSymbol => Int) : (Int, Span.Layout, Int) => Int = {
    val evals = params.map(p => evalParam(simp(p), f))
    (param : Int, layout : Span.Layout, i : Int) => evals(i)(param, layout)
  }

  private def evalParam(param : ParseParam, f : IndexedSymbol => Int) : (Int, Span.Layout) => Int = {
    param match {
      case Nil => (p : Int, layout : Span.Layout) => NIL
      case Const(NIL) => (p : Int, layout : Span.Layout) => NIL
      case Const(c) => (p : Int, layout : Span.Layout) => c
      case Current => (p : Int, layout : Span.Layout) => p
      case LayoutEntity(e) => 
        val i = f(e.s)
        val q = e.q
        (p : Int, layout : Span.Layout) => {
          q.get(layout(i)) match {
            case None => NIL
            case Some(x) => x
          } 
        }
      case Add(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : Int, layout : Span.Layout) => U(p, layout) + V(p, layout)
      case Sub(u, v) => 
        val U = evalParam(u, f)
        val V = evalParam(v, f)
        (p : Int, layout : Span.Layout) => U(p, layout) - V(p, layout)
      case Neg(u) =>
        val U = evalParam(u, f)
        (p : Int, layout : Span.Layout) => - U(p, layout)      
      case Alternative(preferred, alternative) =>
        val ep = evalParam(preferred, f)
        val ea = evalParam(alternative, f)
        (p : Int, layout : Span.Layout) => {
          val u = ep(p, layout)
          if (u < 0) ea(p, layout) else u
        }
    }
  }

  private def simp(p : ParseParam) : ParseParam = {
    p match {
      case Nil => Const(NIL)
      case Current => p
      case _ : Const => p
      case _ : LayoutEntity => p
      case Neg(p) => 
        simp(p) match {
          case Const(c) => Const(-c)
          case Neg(p) => p
          case p => Neg(p)
        }
      case Add(p, q) =>
        (simp(p), simp(q)) match {
          case (Const(u), Const(v)) => Const(u + v)
          case (p, q) => Add(p, q)
        }
      case Sub(p, q) =>
        (simp(p), simp(q)) match {
          case (Const(u), Const(v)) => Const (u - v)
          case (p, q) => Sub(p, q)
        }
      case Alternative(p, q) =>
        (simp(p), simp(q)) match {
          case (Nil, q) => q
          case (Const(c), q) => if (c < 0) q else Const(c)
          case (p, q) => Alternative(p, q)
        }
    }
  }

}


