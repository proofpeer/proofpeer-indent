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
  
  private def i2s(i:(Int, Int)) : String = {
    if (i._1 == i._2) 
      ""+i._1 
    else
      ""+i._1+"-"+i._2
  }
  
  override def toString() : String = {
    if (intervals.isEmpty) 
      "[]"
    else if (intervals.size == 1)
      i2s(intervals.head)
    else {
      var output = "[" + i2s(intervals.head)
      for (i <- intervals.tail) output += (", " + i2s(i))
      output + "]"
    }
  }
  
  override def hashCode() : Int = intervals.hashCode
  
  override def equals(that : Any) : Boolean = intervals.equals(that.asInstanceOf[Range].intervals)
  
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
  
  def outside_interval(left : Int, right : Int) : Range = {
    add(interval(Int.MinValue, left - 1), interval(right + 1, Int.MaxValue))
  }
    
  def singleton(x : Int) = new Range(List((x, x)))
      
}