package proofpeer.indent

sealed class Range private (val intervals : List[(Int, Int)]) {
    
  def isUniversal : Boolean = intervals.isEmpty
  
  def contains(x : Int) : Boolean = {
      if (isUniversal) return true
      for ((left, right) <- intervals) {
        if (left <= x && x <= right) return true
      }
      false
  } 
  
  override def toString() = intervals.toString
  
}

object Range {
  
  def universal : Range = new Range(List())
  
  def add(range : Range, ranges : Range*) : Range = {
    var r = range
    val it = ranges.iterator
    while (!r.isUniversal && it.nonEmpty) {
      val s = it.next
      r = if (s.isUniversal) universal else new Range(r.intervals ++ s.intervals)
    }
    return r
  }
  
  def interval(left : Int, right : Int) : Range = {
    if (left <= right) {
      new Range(List((left, right)))
    } else {
      throw new RuntimeException("invalid interval ("+left+", "+right+")")
    }
  }  
    
  def singleton(x : Int) = new Range(List((x, x)))
  
}