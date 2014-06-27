package proofpeer.indent.test

import proofpeer.indent._
import proofpeer.indent.API._
import proofpeer.indent.APIConversions._
import proofpeer.indent.Constraints._
import proofpeer.indent.Derivation

object Example {

  var priority = 0

  def lrule(l : Nonterminal, r : String) : Grammar = {
    priority = priority + 1
    lexrule(l, r, LexicalPriority(0, Some (priority)))
  }

  def g_letters_and_digits = 
    lrule("Letter", "97-122") ++ 
    lrule("Letter", "65-90") ++
    lrule("Digit", "48-57")

  def g_prog1 = (
       g_letters_and_digits 
    
    ++ rule("E", "App") 
    
    ++ rule("App", "E App")         
    ++ rule("App", "Id") 
    
    ++ lrule("Id", "Letter IdA")
    ++ lrule("IdA", "")
    ++ lrule("IdA", "Digit IdA")
    ++ lrule("IdA", "Letter IdA")
  )
  
  def g_prog2 = (
       g_letters_and_digits 
    
    ++ rule("E", "App") 
    
    ++ rule("App", "E App")         
    ++ rule("App", "Id") 
    
    ++ lrule("Id", "Letter")
    ++ lrule("Id", "Id Letter")
    ++ lrule("Id", "Id Digit")
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
   
   ++ lrule("Id", "Letter")
   ++ lrule("Id", "Id Letter")
   ++ lrule("Id", "Id Digit")
   
   ++ lrule("Num", "Digit")
   ++ lrule("Num", "Digit Num")
   
   ++ lrule("Nil", "110 105 108")
   ++ lrule("Null", "110 117 108_1 108_2")
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
  
  def parsesUniquely(grammar : Grammar, document : String, start : Nonterminal) : Boolean = {
   val g = grammar.parser.parse(document, start)
    g match {
      case None => 
        println("Does not parse.")
        false
      case Some(v) => 
        if (v.isUnique)
          true
        else {
          println("ambiguous parse tree:\n"+Derivation.visualize(grammar, v))
          false
        }
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
    var s = ""
    def p(m : String) { s = s + m + "\n"}
    test(p)
    s
  }
  
  def main(args : Array[String]) {
  }
        
}