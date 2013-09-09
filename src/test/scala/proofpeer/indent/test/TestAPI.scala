package proofpeer.indent.test

import proofpeer.indent._
import proofpeer.indent.test.Example.{check => test}
import proofpeer.indent.test.Example._

import API._
import APIConversions._
import Constraints._
import proofpeer.indent.Derivation

import org.scalacheck.Properties

object TestAPI extends Properties("API") {
    
  property("example0") = test(g_prog2, "a 1", "E", null)
  property("example1") = test(g_prog2, ex1, "E", result1)  
  property("example2") = test(g_prog2, ex2, "E", result2)  
  property("example3") = test(g_prog2, ex3, "E", result3)  
  property("example5") = test(g_prog1, ex5, "E", result5)  
  property("example6") = test(g_prog3, ex6, "E", result6)  
  property("example7") = test(g_prog3, ex7, "E", result7)  
  property("example8") = test(g_prog3, ex8, "E", result8)  
       
}