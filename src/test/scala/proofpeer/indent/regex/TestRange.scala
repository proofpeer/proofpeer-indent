package proofpeer.indent.regex

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TestRange extends Properties("Range") {
 
  property("ex 1") = Range.universal == - Range.empty
  property("ex 2") = Range.empty == - Range.universal
  property("min complement") = - Range(Int.MinValue) == Range(Int.MinValue + 1, Int.MaxValue)
  property("max complement") = - Range(Int.MaxValue) == Range(Int.MinValue, Int.MaxValue - 1)
  property("ex 3") = - (- Range(7)) == Range(7)
  property("ex 4") = forAll { i : Int => - (- Range (i)) == Range(i) }
  property("ex 5") = forAll { (i : Int, j : Int) => i <= j ==> (- (- Range (i, j)) == Range(i, j)) }
  property("ex 6") = forAll { (i : Int, j : Int) => i != j ==> (Range(i) * Range(j) == Range())}
  property("ex 7") = Range(1) + Range(10) + Range(2) + Range(11) + Range(9) + Range(3) == Range(1, 3) + Range(9, 11)
  property("ex 8") = Range(1, 3) + Range(9, 11) == Range(1, 11) - Range(4, 8)
  property("ex 9") = forAll { (i : Int, j : Int) => (-Range(i)) * (-Range(j)) == - (Range(j) + Range(i))}
  property("ex 10") = 
    forAll { xs : List[Int] => 
      var left = Range()
      for (x <- xs) left += Range(x)  
      var right = Range.universal
      for (x <- xs) right = right * (- Range(x))
      - left == right 
    }  

}