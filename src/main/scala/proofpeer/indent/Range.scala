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
  
  def add(range1 : Range, range2 : Range) : Range = {
    if (range1.isUniversal || range2.isUniversal) {
      universal
    } else {
      new Range(range1.intervals ++ range2.intervals)
    }
  }
  
  def interval(left : Int, right : Int) : Range = {
    if (left <= right) {
      new Range(List((left, right)))
    } else {
      throw new RuntimeException("invalid interval ("+left+", "+right+")")
    }
  }  
  
}