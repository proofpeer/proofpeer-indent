package proofpeer.indent

import scalaz._
import Scalaz.{ char => _, _ }

object Util {
  def oneOf[M: Monoid](f: Char => M, xs: String): M = xs.toList foldMap f

  def unfoldM[M[_]:Monad,W:Monoid,A](x: A)(f: A => M[Option[(A,W)]]): M[W] = {
    f(x) >>= {
      case None          => ∅[W].point[M]
      case (Some((y,w))) => unfoldM(y)(f).map {w ⊹ _}
    }
  }

  def unfoldW[W:Monoid,A](x: A)(f: A => Option[(A,W)]): W =
    unfoldM[Id,W,A](x)(f)

  def splitOn(delims: List[Char], str:String) = {
    val positions = delims.map(str.indexOf(_)).filter(_ >= 0)
    if (positions.isEmpty)
      (None,str,"")
    else {
      val pos = positions.min
      (Some(str.charAt(pos)),str.substring(0,pos), str.substring(pos+1,str.length))
    }
  }
}
