/*
  Author: Vineeth Kashyap
  Email: vineeth@cs.ucsb.edu
  In this file, the parser-combinator for Caisson is specified
*/

import scala.util.parsing.combinator._

class CaissonParser extends RegexParsers {
    def prog: Parser[Program] = "prog"~>ident~"("~repsep(ident, ",")~")"~"="~declarations~"in"~definition ^^ {case name~"("~params~")"~"="~decl~"in"~defn => 
                                                                                                                new Program(name, params, decl, defn)}
    
    def declarations: Parser[List[DataDeclaration]] = rep1(dataDeclaration<~";") ^^ ((lst: List[List[DataDeclaration]]) => 
                                                                                       lst.reduceLeft((a: List[DataDeclaration], b: List[DataDeclaration]) => a ++ b))
    
    def dataDeclaration: Parser[List[DataDeclaration]] = dataStructure~rep1sep(pair, ",") ^^ { case ds~lst => lst.map((x => new DataDeclaration(ds, x._1, x._2))) }
    
    def pair: Parser[Tuple2[String, String]] = ident~":"~ident ^^ { case a~":"~b => (a, b) }
    
    def dataStructure: Parser[DataStructure] = dataType~opt(dataSize) ^^ { case dt~ds => new DataStructure(dt, ds) }
    
    def dataType: Parser[DataType] = ( "input" ^^ (_ => Input()) 
                                     | "output" ^^ (_ => Output()) 
                                     | "reg" ^^ (_ => Register()) 
                                     | "inout" ^^ (_ => Inout()) 
                                     | "imem" ^^ (_ => Imem())
                                     | "dmem" ^^ (_ => Dmem())
                                     | "wire" ^^ (_ => Wire()) )
    
    def dataSize: Parser[Tuple2[Int, Int]] = "["~>wholeNumber~":"~wholeNumber<~"]" ^^ { case a~":"~b => (a.toInt, b.toInt) }
    
    def definition: Parser[Definition] = ("let"~>rep1(stateDefinition)~"in"~command ^^ { case sdList~"in"~cmd => LetDefinition(sdList, cmd) }
                                  | command)
    
    def stateDefinition: Parser[StateDefinition] = "state"~>pair~"("~varTypedList~")"~opt(constraintList)~    
                                       "="~"{"~definition<~"}" ^^ { case p~"("~vmap~")"~clist~"="~"{"~defs => new StateDefinition(p._1, p._2, vmap, clist, defs) }

    // :\ stands for foldRight
    def varTypedList: Parser[List[Tuple2[String, String]]] = repsep(ident~":"~ident,",") ^^ (lst => (lst :\ List[Tuple2[String, String]]())((e, l) => e match {case a~":"~b => (a,b) :: l}))
    
    def constraintList: Parser[List[Tuple2[String,String]]] = "["~>repsep(ident~"<"~ident,",")<~"]" ^^ (lst => lst.map((x => x match { case a~"<"~b => (a,b) })))
    
    def command: Parser[Command] = rep1(statement<~";" | branch | kase) ^^ (Command)
    
    def statement: Parser[Statement] = assignment | branch | jump | "fall" ^^ (_ => Fall(None)) | "skip" ^^ (_ => Skip()) | kase
    
    def assignment: Parser[Assignment] = ident~":="~expr ^^ { case lv~":="~rv => Assignment(lv, rv) }

    def expr: Parser[Expr] = ( arithExpr~condOp~arithExpr ^^ { case left~op~right => ComplexExpr(left, right, op) }
                                | "("~>arithExpr~condOp~arithExpr<~")" ^^ { case left~op~right => ComplexExpr(left, right, op) }
                                | arithExpr
                                | "!"~expr ^^ { case unop~e => UnaryExpr(unop, e) } )
        
    def arithExpr: Parser[Expr] = term~rep(binop~term)  ^^ { case t~lt => lt.foldLeft(t)((left, right) => right match { case op~rt => ComplexExpr(left, rt, op) }) }
   
    
    def term: Parser[Expr] = ( verilogNumber ^^ (Number) 
                            | ident               ^^ (Variable)
                            | "!"~arithExpr ^^ { case unop~e => UnaryExpr(unop, e) }                            
                            | "("~>arithExpr<~")" )      

    def binop: Parser[String] = "+" | "-" | "&&" | "||" | "<<" | ">>" | "*" | "/"
                                
    def condOp: Parser[String] = "==" | "<" | ">" | "<=" | ">=" | "!="
                                        
    def branch: Parser[Branch] = "if"~>expr~"then"~"{"~command~"}"~opt("else"~"{"~command~"}") ^^ { case cond~"then"~"{"~tbody~"}"~Some("else"~"{"~ebody~"}") => 
                                                                                                                                        Branch(cond, tbody, Some(ebody))
                                                                                                    case cond~"then"~"{"~tbody~"}"~None => Branch(cond, tbody, None) }
    
    def kase: Parser[Kase] = caseHeader~"{"~rep1(caseBody)<~"}" ^^ { case cond~"{"~cList => Kase(cond, cList.reduceLeft((a, b) => a ++ b)) }
    
    def caseBody: Parser[Map[String, Command]] = (verilogNumber | "default")~":"~"{"~command<~"}" ^^ { case key~":"~"{"~c => Map(key -> c) }
    
    def caseHeader: Parser[Expr] = "case"~"("~expr<~")" ^^ { case "case"~"("~e => e }
    
    def jump: Parser[Jump] =  "goto"~>ident~"("~repsep(ident, ",")<~")" ^^ { case target~"("~varList => Jump(target, varList) }
    
    def verilogNumber: Parser[String] = (binaryNumber | floatingPointNumber)

    def ident: Parser[String] = """[a-zA-Z_]\w*""".r
    
    def wholeNumber: Parser[String] = """-?\d+""".r
    
    def decimalNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)""".r

    def stringLiteral: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
    
    def floatingPointNumber: Parser[String] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    
    def binaryNumber: Parser[String] = """\d+\'b[01]+""".r
}

class CaissonParseException(msg: String) extends Exception {
  def message: String = msg
}

/* For testing
import java.io.FileReader

object ParserTester {
    def main(args: Array[String]) {
        if (args.length != 1) { 
          println("Please provide caisson filename to be compiled")
          exit(-1)
        }
        val reader = new FileReader(args(0))
        val parser = new CaissonParser()
        println(parser.parseAll(parser.kase, reader))
    }
} */

