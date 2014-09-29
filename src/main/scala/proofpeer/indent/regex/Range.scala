package proofpeer.indent.regex

/** Implements sets of integers such that ranges of integers and their complements are represented efficiently.
  *
  * To achieve this, the set is represented canonically as list of closed intervals:
  *  - For each interval (left, right) in the list left <= right holds.
  *  - If (left1, right1) occurs in the list before (left2, right2), then right1 + 1 < left2
  *
  * '''Examples:'''
  *  - `Range(2, 5) + Range(10, 12)` is the set of numbers 2, 3, 4, 5, 10, 11, 12
  *  - `(- Range(7)) * (- Range(13))` is the set of all (32-bit) integers except 7 and 13
  *  - `- (Range(7) + Range(13))` also is the set of all (32-bit) integers except 7 and 13
  */
sealed class Range private (val intervals : Vector[Range.Interval]) {
      
  def isEmpty : Boolean = intervals.isEmpty

  /** Checks if an integer belongs to this set or not. */
  def contains(x : Int) : Boolean = {
    for ((left, right) <- intervals) {
      if (left <= x && x <= right) return true
    }
    false
  } 
  
  private def i2s(i:(Int, Int)) : String = 
    if (i._1 == i._2) ""+i._1 else "("+i._1+", "+i._2+")"
  
  override def toString() : String = {
    var sep = "["
    var buf : String = ""
    for (i <- intervals) {
      buf = buf + sep + i2s(i)
      sep = ", "
    }
    buf + "]"
  }
  
  override def hashCode() : Int = intervals.hashCode
  
  /** Equality means set equality. */
  override def equals(that : Any) : Boolean = 
    that match {
      case r : Range => intervals.equals(r.intervals)
      case _ => false
    }

  /** Set union of this range with the argument range. */
  def +(range : Range) : Range = { 
    var index = 0
    var intervals = this.intervals
    for ((left, right) <- range.intervals) {
      val (newIntervals, newIndex) = Range.addInterval(intervals, index, left, right)
      intervals = newIntervals
      index = newIndex
    }
    new Range(intervals)
  }

  /** The complement of this set. */
  def unary_- : Range = {
    var ivs : Vector[Range.Interval] = Vector()
    var l = Int.MinValue
    for ((left, right) <- intervals) {
      if (l < left) ivs :+= (l, left-1)
      l = right + 1 
    }
    if (l != Int.MinValue || intervals.isEmpty) ivs :+= (l, Int.MaxValue)
    new Range(ivs)
  }

  /** Set intersection of this range with the argument range. */
  def *(range : Range) : Range = {
    var ivs : Vector[Range.Interval] = Vector()
    val ivs1 = intervals
    val ivs2 = range.intervals
    var (size1, index1) = (ivs1.size, 0)
    var (size2, index2) = (ivs2.size, 0)
    while (index1 < size1 && index2 < size2) {
      val (left1, right1) = ivs1(index1)
      val (left2, right2) = ivs2(index2)
      if (right1 < left2) 
        index1 += 1
      else if (right2 < left1)
        index2 += 1
      else {
        // left1 <= right2 && right1 >= left2
        if (left1 <= left2) {
          if (right1 < right2) {
            ivs :+= (left2, right1)
            index1 += 1
          } else {
            ivs :+= (left2, right2)
            index2 += 1
          }
        } else {
          if (right1 < right2) {
            ivs :+= (left1, right1)
            index1 += 1
          } else {
            ivs :+= (left1, right2)
            index2 += 1
          }
        }
      }
    }
    new Range(ivs)
  }

  /** Set difference between this range and the argument range. */
  def - (range : Range) : Range = this * (- range)

}

object Range {

  type Interval = (Int, Int)
  
  /** The set of all (32-bit) integers. */
  val universal : Range = Range(Int.MinValue, Int.MaxValue)

  /** The empty set. */
  val empty : Range = new Range(Vector())

  // helper function for +
  private def addInterval(intervals : Vector[Interval], index : Int, left : Int, right : Int) : 
    (Vector[Interval], Int)  = 
  {
    var i = index
    val size = intervals.size
    while (i < size) {
      val (l, r) = intervals(i)
      if (r < Int.MaxValue && left > r + 1) i += 1
      else {
        val newLeft = if (left < l) left else l
        var j = i
        while (j < size && right > intervals(j)._2) j += 1
        if (j >= size || right == Int.MaxValue) 
          return (intervals.take(i) :+ (newLeft, right), i + 1)
        else if (right + 1 >= intervals(j)._1) 
          return ((intervals.take(i) :+ (newLeft, intervals(j)._2)) ++ intervals.drop(j + 1), i)
        else 
          return ((intervals.take(i) :+ (newLeft, right)) ++ intervals.drop(j), i + 1)
      }        
    }
    (intervals :+ (left, right), size + 1)
  }

  /** The set consisting of all x such that left <= x <= right. */
  def apply(left : Int, right : Int) : Range = {
    if (left <= right) {
      new Range(Vector((left, right)))
    } else {
      throw new RuntimeException("invalid interval ("+left+", "+right+")")
    }
  }

  /* The singleton set consisting of just x. */
  def apply(x : Int) : Range = new Range(Vector((x, x)))

  /* The empty set. */
  def apply() : Range = empty
        
}