package proofpeer.indent

import proofpeer.indent.regex._

import scalaz._
import Scalaz.{ char => _, _ }

object Instances {
  implicit object RegularExprIsMonoid extends Monoid[RegularExpr] {
    override def append(x: RegularExpr,y: =>RegularExpr) = ALT(x,y)
    override def zero = NOTHING
  }
  implicit object RangeIsMonoid extends Monoid[Range] {
    override def append(x: Range,y: =>Range) = x + y
    override def zero = Range.empty
  }
  implicit object GrammarIsMonoid extends Monoid[Grammar] {
    override def append(x: Grammar,y: =>Grammar) = x ++ y
    override def zero = new Grammar(Vector())
  }
}
