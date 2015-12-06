package proofpeer.indent.l3paper

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.earley._
import java.io.File

object Tools {

  def r(nonterminal : String, rhs : String) : Grammar = 
    rule(nonterminal, rhs, c => null)

  val NO_PRIO : Option[Int] = None
  val SCOPE_1 = "Scope 1"
  val SCOPE_2 = "Scope 2"

  var outputDir : File = null

  def run(title : String, grammar : Grammar, nonterminal : String, s : String) {
    println("processing '" + title + "'")
    val buffer = new StringBuilder()
    def writeln(s : String) {
      if (outputDir == null) println(s) else {
        buffer.append(s)
        buffer.append("\n")        
      }
    }
    val parser = Parser(grammar)
    writeln("// " + title + ": " + nonterminal + " =>* " + s)
    parser.parseAsTree(nonterminal, s) match {
      case None => println("// cannot parse")
      case Some((document, tree)) =>
        writeln("// number of trees in graph: " + tree.countTrees)
        val graph = PrettyParseTrees.mkGraph(document, tree)
        val asDot = PrettyParseTrees.asDot(title, graph)
        writeln(asDot)
    }
    writeln("")
    if (outputDir != null)
      writefile(outputDir, title + ".dot", buffer.toString)     
  }

  def writefile(dir : File, filename : String, text : String) {
    import java.io._
    val pw = new PrintWriter(new File(dir, filename))
    pw.write(text)
    pw.close    
  }

}

object Examples {

  import Tools._

  val grammar_0 =
    rule("id", REPEAT1(chars('a', 'z')), Some(1), SCOPE_1) ++
    rule("minus", string("-"), NO_PRIO, SCOPE_1) ++
    rule("plus", string("+"), NO_PRIO, SCOPE_1) ++
    r("S", "S minus A") ++
    r("S", "S plus A") ++
    r("S", "A") ++
    r("A", "A E") ++
    r("A", "E") ++
    r("E", "id")

  val grammar_1 = grammar_0 ++
    rule("symbol", REPEAT1(ALT(char('-'), chars('a', 'z'))), NO_PRIO, SCOPE_2) ++
    r("E", "symbol")

  val grammar_2 = grammar_0 ++
    rule("symbol", REPEAT1(ALT(char('-'), chars('a', 'z'))), NO_PRIO, SCOPE_1) ++
    r("E", "symbol")

  val grammar_3 = grammar_0 ++
    rule("symbol", REPEAT1(ALT(char('-'), chars('a', 'z'))), Some(0), SCOPE_1) ++
    r("E", "symbol")

  val lexerhack = 
    rule("id", REPEAT1(chars('a', 'z'))) ++
    rule("typeid", REPEAT1(chars('a', 'z'))) ++
    rule("asterisk", string("*")) ++
    rule("left", string("(")) ++
    rule("right", string(")")) ++
    r("Expr", "Cast") ++
    r("Expr", "Mul") ++
    r("Expr", "Deref") ++
    r("Expr", "id") ++
    r("Expr", "left Expr right") ++
    r("Mul", "Expr asterisk Expr") ++
    r("Cast", "left Type right Expr") ++
    r("Type", "typeid") ++
    r("Deref", "asterisk Expr")


  def _main(args : Array[String]) {
    outputDir = new File("/Users/stevenobua/parsing/dots")
    run("Example_0", grammar_0, "S", "a-b+c")
    run("Example_1", grammar_1, "S", "a-b+c")
    run("Example_2", grammar_2, "S", "a-b+c")
    run("Example_3", grammar_3, "S", "a-b+c")
    run("lexerhack", lexerhack, "Expr", "(a)*b")

  }

}


