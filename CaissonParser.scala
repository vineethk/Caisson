import scala.util.parsing.combinator._
import java.io.FileReader

//fix Parser[Any] to specific types

sealed abstract class Expr 
case class Number(value: Float) extends Expr 
case class Variable(name: String) extends Expr
case class ComplexExpr(left: Expr, right: Expr, op: String) extends Expr

sealed abstract class Statement
case class Assignment(lvalue: String, rvalue: Expr) extends Statement
case class Branch(cond: Expr, thenBody: List[Statement], elseBody: Option[List[Statement]]) extends Statement
case class Jump(target: String, argList: List[String]) extends Statement
case class Fall() extends Statement
case class Skip() extends Statement


class ParseCaisson extends JavaTokenParsers {
    def prog: Parser[Any] = "prog"~ident~"("~repsep(ident, ",")~")"~"="~declarations~"in"~definition
    
    def declarations: Parser[Any] = rep1(dataDeclaration~";")
    
    def dataDeclaration: Parser[Any] = dataStructure~rep1sep(pair, ",")
    
    def pair: Parser[Any] = ident~":"~ident 
    
    def dataStructure: Parser[Any] = dataType~opt(dataSize)
    
    def dataType: Parser[Any] = "input" | "output" | "reg" | "inout"
    
    def dataSize: Parser[Any] = "["~wholeNumber~":"~wholeNumber~"]"
    
    def definition: Parser[Any] = "let"~rep1(stateDefinition)~"in"~command | command
    
    def stateDefinition: Parser[Any] = "state"~pair~"("~varTypedList~")"~opt(constraintList)~    
                                       "="~"{"~definition~"}"

    def varTypedList: Parser[Any] = repsep(ident~opt(":"~ident),",")
    
    def constraintList: Parser[Any] = "["~repsep(ident~"<"~ident,",")~"]"
    
    def command: Parser[List[Statement]] = rep1(statement<~";" | branch) 
    
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
                                        
    def branch: Parser[Branch] = "if"~>expr~"then"~"{"~command~"}"~opt("else"~"{"~command~"}") ^^ { case cond~"then"~"{"~tbody~"}"~Some("else"~"{"~ebody~"}") => Branch(cond, tbody, Some(ebody))
                                                                                                 case cond~"then"~"{"~tbody~"}"~None => Branch(cond, tbody, None) }
    
    def jump: Parser[Jump] =  "goto"~>ident~"("~repsep(ident, ",")<~")" ^^ { case target~"("~varList => Jump(target, varList) }
                  
}

object CaissonParser extends ParseCaisson {
    def main(args: Array[String]) {
        val reader = new FileReader(args(0))
        println(parseAll(prog, reader))
 }
}
