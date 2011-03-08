import scala.util.parsing.combinator._
import java.io.FileReader

sealed abstract class Expr 
case class Number(value: Float) extends Expr 
case class Variable(name: String) extends Expr
case class ComplexExpr(left: Expr, right: Expr, op: String) extends Expr

sealed abstract class Statement
case class Assignment(lvalue: String, rvalue: Expr) extends Statement
case class Branch(cond: Expr, thenBody: Command, elseBody: Option[Command]) extends Statement
case class Jump(target: String, argList: List[String]) extends Statement
case class Fall() extends Statement
case class Skip() extends Statement

sealed abstract class Definition
case class LetDefinition(stateDefList: List[StateDefinition], cmd: Command)  extends Definition
case class Command(stmtList: List[Statement]) extends Definition

class StateDefinition(name: String, secLevel: String, paramMap: Map[String,String], constraintList: Option[List[Tuple2[String,String]]], definition: Definition)

sealed abstract class DataType
case class Input() extends DataType
case class Output() extends DataType
case class Register() extends DataType
case class Inout() extends DataType

class DataStructure(dType: DataType, dimension: Option[Tuple2[Int, Int]])

class DataDeclaration(dStructure: DataStructure, name: String, level: String)

class Program(name: String, params: List[String], decl: List[DataDeclaration], defn: Definition)

class ParseCaisson extends JavaTokenParsers {
    def prog: Parser[Program] = "prog"~>ident~"("~repsep(ident, ",")~")"~"="~declarations~"in"~definition ^^ {case name~"("~params~")"~"="~decl~"in"~defn => 
                                                                                                                new Program(name, params, decl, defn)}
    
    def declarations: Parser[List[DataDeclaration]] = rep1(dataDeclaration<~";") ^^ ((lst: List[List[DataDeclaration]]) => 
                                                                                       lst.reduceLeft((a: List[DataDeclaration], b: List[DataDeclaration]) => a ++ b))
    
    def dataDeclaration: Parser[List[DataDeclaration]] = dataStructure~rep1sep(pair, ",") ^^ { case ds~lst => lst.map((x => new DataDeclaration(ds, x._1, x._2))) }
    
    def pair: Parser[Tuple2[String, String]] = ident~":"~ident ^^ { case a~":"~b => (a, b) }
    
    def dataStructure: Parser[DataStructure] = dataType~opt(dataSize) ^^ { case dt~ds => new DataStructure(dt, ds) }
    
    def dataType: Parser[DataType] = "input" ^^ (_ => Input()) | "output" ^^ (_ => Output()) | "reg" ^^ (_ => Register()) | "inout" ^^ (_ => Inout())
    
    def dataSize: Parser[Tuple2[Int, Int]] = "["~>wholeNumber~":"~wholeNumber<~"]" ^^ { case a~":"~b => (a.toInt, b.toInt) }
    
    def definition: Parser[Definition] = ("let"~>rep1(stateDefinition)~"in"~command ^^ { case sdList~"in"~cmd => LetDefinition(sdList, cmd) }
                                  | command)
    
    def stateDefinition: Parser[StateDefinition] = "state"~>pair~"("~varTypedList~")"~opt(constraintList)~    
                                       "="~"{"~definition<~"}" ^^ { case p~"("~vmap~")"~clist~"="~"{"~defs => new StateDefinition(p._1, p._2, vmap, clist, defs) }

    def varTypedList: Parser[Map[String, String]] = repsep(ident~":"~ident,",") ^^ (lst => lst.foldLeft(Map[String, String]())((m, e) => e match {case a~":"~b => m + (a -> b)}))
    
    def constraintList: Parser[List[Tuple2[String,String]]] = "["~>repsep(ident~"<"~ident,",")<~"]" ^^ (lst => lst.map((x => x match { case a~"<"~b => (a,b) })))
    
    def command: Parser[Command] = rep1(statement<~";" | branch) ^^ (Command)
    
    def statement: Parser[Statement] = assignment | branch | jump | "fall" ^^ (_ => Fall()) | "skip" ^^ (_ => Skip())
    
    def assignment: Parser[Assignment] = ident~":="~expr ^^ { case lv~":="~rv => Assignment(lv, rv) }

    def expr: Parser[Expr] = ( arithExpr~condOp~arithExpr ^^ { case left~op~right => ComplexExpr(left, right, op) }
                                | "("~>arithExpr~condOp~arithExpr<~")" ^^ { case left~op~right => ComplexExpr(left, right, op) }
                                | arithExpr)
        
    def arithExpr: Parser[Expr] = term~rep(binop~term)  ^^ { case t~lt => lt.foldLeft(t)((left, right) => right match { case op~rt => ComplexExpr(left, rt, op) }) }
   
    
    def term: Parser[Expr] = ( floatingPointNumber ^^ ((x) => Number(x.toFloat)) 
                            | ident               ^^ (Variable)
                            | "("~>arithExpr<~")")       

    def binop: Parser[String] = "+" | "-"
                                
    def condOp: Parser[String] = "==" | "<"
                                        
    def branch: Parser[Branch] = "if"~>expr~"then"~"{"~command~"}"~opt("else"~"{"~command~"}") ^^ { case cond~"then"~"{"~tbody~"}"~Some("else"~"{"~ebody~"}") => 
                                                                                                                                        Branch(cond, tbody, Some(ebody))
                                                                                                    case cond~"then"~"{"~tbody~"}"~None => Branch(cond, tbody, None) }
    
    def jump: Parser[Jump] =  "goto"~>ident~"("~repsep(ident, ",")<~")" ^^ { case target~"("~varList => Jump(target, varList) }
                  
}

object CaissonParser extends ParseCaisson {
    def main(args: Array[String]) {
        val reader = new FileReader(args(0))
        println(parseAll(prog, reader))
 }
}
