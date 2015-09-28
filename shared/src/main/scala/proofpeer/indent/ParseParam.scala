package proofpeer.indent

sealed trait ParseParam 

object ParseParam {

  final case object Nil extends ParseParam
  final case class Const(c : Int) extends ParseParam
  final case object Current extends ParseParam
  final case class LayoutEntity(e : Constraint.LayoutEntity) extends ParseParam
  final case class Add(p : ParseParam, c : Int) extends ParseParam
  final case class Alternative(preferred : ParseParam, alternative : ParseParam) extends ParseParam

  private val P = earley.Earley.DEFAULT_PARAM

  if (P >= 0) throw new RuntimeException("default parse parameter must be negative")

  def noParams(l : Int) : Vector[ParseParam] = {
    Vector.fill(l)(Nil)
  }

  def collectSymbols(param : ParseParam) : Set[IndexedSymbol] = {
    param match {
      case LayoutEntity(e) => Set(e.s)
      case Add(p, c) => collectSymbols(p)
      case Alternative(a, b) => collectSymbols(a) ++ collectSymbols(b)
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
    val evals = params.map(p => evalParam(p, f))
    (param : Int, layout : Span.Layout, i : Int) => evals(i)(param, layout)
  }

  def evalParam(param : ParseParam, f : IndexedSymbol => Int) : (Int, Span.Layout) => Int = {
    param match {
      case Nil => (p : Int, layout : Span.Layout) => P
      case Const(c) => (p : Int, layout : Span.Layout) => c
      case Current => (p : Int, layout : Span.Layout) => p
      case LayoutEntity(e) => 
        val i = f(e.s)
        val q = e.q
        (p : Int, layout : Span.Layout) => {
          q.get(layout(i)) match {
            case None => P
            case Some(x) => x
          } 
        }
      case Add(param, c) => 
        val e = evalParam(param, f)
        (p : Int, layout : Span.Layout) => e(p, layout) + c
      case Alternative(preferred, alternative) =>
        val ep = evalParam(preferred, f)
        val ea = evalParam(alternative, f)
        (p : Int, layout : Span.Layout) => {
          val u = ep(p, layout)
          if (u < 0) ea(p, layout) else u
        }
    }
  }

}


