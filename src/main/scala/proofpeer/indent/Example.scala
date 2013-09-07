package proofpeer.indent

import API._
import APIConversions._
import Constraints._

object Example {

  def g_letters_and_digits = 
    lexrule("Letter", "97-122") ++ 
    lexrule("Letter", "65-90") ++
    lexrule("Digit", "48-57")

  def g_prog1 = (
       g_letters_and_digits 
    
    ++ rule("E", "App") 
    
    ++ rule("App", "E App")         
    ++ rule("App", "Id") 
    
    ++ lexrule("Id", "Letter IdA")
    ++ lexrule("IdA", "")
    ++ lexrule("IdA", "Digit IdA")
    ++ lexrule("IdA", "Letter IdA")
  )
  
  def g_prog2 = (
       g_letters_and_digits 
    
    ++ rule("E", "App") 
    
    ++ rule("App", "E App")         
    ++ rule("App", "Id") 
    
    ++ lexrule("Id", "Letter")
    ++ lexrule("Id", "Id Letter")
    ++ lexrule("Id", "Id Digit")
  )
  
  def g_prog3 = (
      g_letters_and_digits
      
   ++ rule("E", "App")
   
   ++ rule("App", "Atomic App")
   ++ rule("App", "Atomic")
   
   ++ rule("Atomic", "Id")
   ++ rule("Atomic", "Num")
   ++ rule("Atomic", "Nil")
   ++ rule("Atomic", "Null")
   
   ++ lexrule("Id", "Letter")
   ++ lexrule("Id", "Id Letter")
   ++ lexrule("Id", "Id Digit")
   
   ++ lexrule("Num", "Digit")
   ++ lexrule("Num", "Digit Num")
   
   ++ lexrule("Nil", "110 105 108")
   ++ lexrule("Null", "110 117 108_1 108_2")
  )

  def ex1 = "abcdef"
  def result1 = """
E => App
  App => Id
    Id => Id Letter
      Id => Id Letter
        Id => Id Letter
          Id => Id Letter
            Id => Id Letter
              Id => Letter
                Letter => 97
              Letter => 98
            Letter => 99
          Letter => 100
        Letter => 101
      Letter => 102"""
  
  def ex2 = "a b"
  def result2 = """
E => App
  App => E App
    E => App
      App => Id
        Id => Letter
          Letter => 97
    App => Id
      Id => Letter
        Letter => 98""" 
    
  def ex3 = "a a a"
  def result3 = """
E => App
  ==========================
  App => E App
    E => App
      App => E App
        E => App
          App => Id
            Id => Letter
              Letter => 97
        App => Id
          Id => Letter
            Letter => 97
    App => Id
      Id => Letter
        Letter => 97
  --------------------------
  App => E App
    E => App
      App => Id
        Id => Letter
          Letter => 97
    App => E App
      E => App
        App => Id
          Id => Letter
            Letter => 97
      App => Id
        Id => Letter
          Letter => 97
  ==========================""" 
    
  def ex5 = "hello"
  def result5 = """
E => App
  App => Id
    Id => Letter IdA
      Letter => 104
      IdA => Letter IdA
        Letter => 101
        IdA => Letter IdA
          Letter => 108
          IdA => Letter IdA
            Letter => 108
            IdA => Letter IdA
              Letter => 111
              IdA => """    
    
  def ex6 = "a a a a a a 10 a10 20"
  def result6 = """
E => App
  App => Atomic App
    Atomic => Id
      Id => Letter
        Letter => 97
    App => Atomic App
      Atomic => Id
        Id => Letter
          Letter => 97
      App => Atomic App
        Atomic => Id
          Id => Letter
            Letter => 97
        App => Atomic App
          Atomic => Id
            Id => Letter
              Letter => 97
          App => Atomic App
            Atomic => Id
              Id => Letter
                Letter => 97
            App => Atomic App
              Atomic => Id
                Id => Letter
                  Letter => 97
              App => Atomic App
                Atomic => Num
                  Num => Digit Num
                    Digit => 49
                    Num => Digit
                      Digit => 48
                App => Atomic App
                  Atomic => Id
                    Id => Id Digit
                      Id => Id Digit
                        Id => Letter
                          Letter => 97
                        Digit => 49
                      Digit => 48
                  App => Atomic
                    Atomic => Num
                      Num => Digit Num
                        Digit => 50
                        Num => Digit
                          Digit => 48
"""
    
  def ex7 = "null nil"
  def result7 = """
E => App
  App => Atomic App
    Atomic => Null
      Null => 110 117 108 108
    App => Atomic
      Atomic => Nil
        Nil => 110 105 108"""    
    
  def ex8 = "null nill"
  def result8 = """
E => App
  App => Atomic App
    Atomic => Null
      Null => 110 117 108 108
    App => Atomic
      Atomic => Id
        Id => Id Letter
          Id => Id Letter
            Id => Id Letter
              Id => Letter
                Letter => 110
              Letter => 105
            Letter => 108
          Letter => 108"""    
        
  def run(grammar : Grammar, document : String, start : Nonterminal) : String = 
  {
    val g = grammar.parser.parse(document, start)
    g match {
      case None => 
        "no valid parse tree"
      case Some(v) => 
        "found parse tree:\n"+Derivation.visualize(grammar, v)
    }    
  }
  
  def check(grammar : Grammar, document : String, start : Nonterminal, 
      result : String, println : String => Unit) : Boolean = 
  {
    val g = grammar.parser.parse(document, start)
    val ok = g match {
      case None => result == null
      case Some(v) =>
        val derivation = Derivation.visualize(grammar, v)
        val lines1 = result.split("\n").filter(_.trim() != "")
        val lines2 = derivation.split("\n").filter(_.trim() != "")
        val ok = lines1.toList == lines2.toList
        if (!ok) {
          println("*** CHECK FAILED ****\n"+derivation)
        }
        ok
    }  
    if (ok) {
      println("Successfully checked '"+document+"'.")
    } else {
      println("Failed to check '"+document+"'.")
    }
    ok
  }
  
  def check(grammar : Grammar, document : String, start : Nonterminal, 
      result : String) : Boolean = 
    check(grammar, document, start, result, println(_)) 
  
    
  def test(p : String => Unit) {
    check(g_prog2, "a 1", "E", null, p)
    check(g_prog2, ex1, "E", result1, p)
    check(g_prog2, ex2, "E", result2, p)
    check(g_prog2, ex3, "E", result3, p)
    check(g_prog1, ex5, "E", result5, p)
    check(g_prog3, ex6, "E", result6, p)
    check(g_prog3, ex7, "E", result7, p)
    check(g_prog3, ex8, "E", result8, p)    
  }
  
  def teststr() : String = {
    bug2()
    var s = ""
    def p(m : String) { s = s + m + "\n"}
    test(p)
    s
  }
  
  def bug1() {
    import scala.collection.immutable._
    def translate(col : Array[String]) : String =
      col.map(_.toString).fold("")(_ + "," + _)
    val ordering = Ordering.by[Array[String], String](translate)
    var collection = SortedSet()(ordering)  
    val one = Array("elem1")
    val two = Array("elem2")
    collection += one
    collection += two
  }

  def bug2() {
    import scala.collection.immutable._
    var collection = SortedSet("hello")
    collection += "world"
  }
  
  
  def main(args : Array[String]) {
    //val p : String => Unit = (s => println(s))
    //test(p)
    println(teststr())
  }
    
}