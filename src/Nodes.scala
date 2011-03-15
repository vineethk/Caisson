type ConstraintList = List[(SimpleType, SimpleType)]

sealed abstract class CaissonASTNode {
  def caissonType(env: Environment): (CaissonType, ConstraintList)
}

sealed abstract class Expr extends CaissonASTNode
case class Number(value: Float) extends Expr {
  def caissonType(env: Environment) = (SimpleType("L"), Util.emptyConstraintList) //implements rule T-CONST
}
case class Variable(name: String) extends Expr {
  def caissonType(env: Environment) = (env.typeMap(name), Util.emptyConstraintList) //implements rule T-REG/VAR
}
case class ComplexExpr(left: Expr, right: Expr, op: String) extends Expr {
  def caissonType(env: Environment) = (TypeUtil.join(left.caissonType(env), right.caissonType(env)), Util.emptyConstraintList) //implements rule T-OP
}

sealed abstract class Statement extends CaissonASTNode {
  def fallTransform(state: String): Statement
}
case class Assignment(lvalue: String, rvalue: Expr) extends Statement {
  def caissonType(env: Environment) = { //implements T-ASSIGN
    val lvalueType = env.typeMap(lvalue)
    (CommandType(lvalueType), List((rvalue.caissonType(env), lvalueType)))
  }

  def fallTransform(state: String) = this
}

case class Branch(cond: Expr, thenBody: Command, elseBody: Option[Command]) extends Statement {
  def caissonType(env: Environment) = { //implements T-IF
    val condType = cond.caissonType(env)
    val thenBodyType = thenBody.caissonType(env)
    val elseBodyType = elseBody match {
      case Some(x) => x.caissonType(env)
      case None => thenBodyType
    }
    val bodyType = TypeUtil.meet(thenBodyType.level, elseBodyType.level)
    (CommandType(bodyType), List((condType, bodyType)))
  }
  def fallTransform(state: String) = {
    val transformedElse = elseBody match {
      case Some(c) => c.fallTransform(state)
      case None => None
    }
    Branch(cond, thenBody.fallTransform(state), transformedElse)
  }
}

case class Jump(target: String, argList: List[String]) extends Statement {
  def fallTransform(state: String) = this

  def caissonType(env: Environment) = { //implements T-GOTO
    val targetStateType = env.typeMap(target)
    assert(targetStateType.paramTypeList.length == argList.length)
    val typeSubstitutionMap = List.range(0, argList.length).map(i => Map(targetStateType.paramTypeList(i).level -> env.typeMap(argList(i)))).reduceLeft((a, b) => a ++ b)
    val sourceType = substituteType(targetStateType.level, typeSubstitutionMap)
    val substitutedConstraints = Util.substituteConstraints(targetStateType.constraints, typeSubstitutionMap)
    (CommandType(sourceType), substitutedConstraints)
  }
}

case class Fall(level: Option[String]) extends Statement {
  def caissonType(env: Environment) = { //implements T-FALL
    val stateLevel = env.typeMap(extractLevel).level
    val defaultChildCommandLevel = env.functions.command(env.functions.default(extractLevel)).caissonType(env).level
    (CommandType(stateLevel), List((stateLevel, defaultChildCommandLevel)))
  }

  private def extractLevel = level match {
    case Some(x) => x
    case None => println("Internal compiler error: Fall not labelled"); exit(-1)
  }

  def fallTransform(state: String) = Fall(Some(state))
}
case class Skip() extends Statement {
  def caissonType(env: Environment) = (CommandType(SimpleType("H")), Util.emptyConstraintList) //implements T-SKIP

  def fallTransform(state: String) = this
}

sealed abstract class Definition extends CaissonASTNode {
  def computeEnvironment(state: String): Environment
  def fallTransform(state: String): Definition
}

case class LetDefinition(stateDefList: List[StateDefinition], cmd: Command)  extends Definition {
  def computeEnvironment(state: String): Environment = {
    val fcmd = Map(state -> cmd)
    val fdef = Map(state -> stateDefList(0).label)
    stateDefList.map(_.computeEnvironment(state)).reduceLeft((a, b) => a+b) + new Environment(Map(), new FunctionMapping(fcmd, fdef))
  }

  def caissonType(env: Environment) = { //implements T-DEF
    val stateConstraints = stateDefList.map(_.caissonType(env)._2).reduceLeft((a, b) => a ++ b)
    val cmdType = cmd.caissonType(env)
    (cmdType._1, stateConstraints ++ cmdType._2)
  }

  def fallTransform(state: String) = LetDefinition(stateDefList.map(_.fallTransform(state)), cmd.fallTransform(state))
}

case class Command(stmtList: List[Statement]) extends Definition {
  def caissonType(env: Environment) = (stmtList.map(x => CommandType(x.caissonType(env).level).reduceLeft((l,r) => TypeUtil.meet(l, r))), Util.emptyConstraintList) //implements T-SEQ

  def computeEnvironment(state: String): Environment = new Environment(Map(), new FunctionMapping(Map(state -> this)), Map())

  def fallTransform(state: String) = Command(stmtList.map(_.fallTransform(state)))
}

class StateDefinition(name: String, secLevel: String, paramAndTypeList: List[(String, String)], constraintList: Option[List(String,String)], definition: Definition) {
  def label = name

  def computeEnvironment(state: String): Environment = {
    val constraints = constraintList match {
      case Some(x) => x.map(a => (SimpleType(a._1), SimpleType(a._2)))
      case None => List[Tuple2[SimpleType, SimpleType]]()
    }
    val stateNameMapping = Map(name -> StateType(SimpleType(secLevel), paramTypeList.map(a => SimpleType(a._2)), constraints))
    (new Environment(stateNameMapping, Map())) + definition.computeEnvironment(name)
  }

  def caissonType(env: Environment) = { //implements T-STATE
    val stateType = env.typeMap(name)
    (stateType, List((stateType.level, definition.caissonType(env).level)))
  }

  def fallTransform(state: String) = new StateDefinition(name, secLevel, paramAndTypeList, constraintList, definition.fallTransform(name))
}

sealed abstract class DataType
case class Input() extends DataType
case class Output() extends DataType
case class Register() extends DataType
case class Inout() extends DataType

class DataStructure(dType: DataType, dimension: Option[Tuple2[Int, Int]])

class DataDeclaration(dStructure: DataStructure, name: String, level: String) {
  def computeEnvironment(state: String): Environment = new Environment(Map(name -> SimpleType(level)), Map())
}

class Program(name: String, params: List[String], decl: List[DataDeclaration], defn: Definition) {
  def computeEnvironment: Environment = decl.map(_.computeEnvironment(name)).reduceLeft((a, b) => a+b) + defn.computeEnvironment(name)

  def fallTransform = new Program(name, params, decl, defn.fallTransform(name))
}

