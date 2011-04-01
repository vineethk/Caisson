/*
  Author: Vineeth Kashyap
  Email: vineeth@cs.ucsb.edu
  This file defines the Abstract Syntax Tree for Caisson Programs. Along with it, different transformations, type checkers, validators are also defined.
 */

sealed abstract class CaissonASTNode {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CaissonType
}

sealed abstract class Expr extends CaissonASTNode {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType
}

case class Number(value: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = SimpleType("L") //implements rule T-CONST
}

case class Variable(name: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = { //implements rule T-REG/VAR
    assert(env.typeMap(name).isInstanceOf[SimpleType])
    env.typeMap(name).asInstanceOf[SimpleType]
  }
}

case class ComplexExpr(left: Expr, right: Expr, op: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = TypeUtil.join(kappa, left.caissonType(env, kappa), right.caissonType(env, kappa)) //implements rule T-OP
}

case class UnaryExpr(operator: String, operand: Expr) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = operand.caissonType(env, kappa)
}

sealed abstract class Statement extends CaissonASTNode {
  def fallTransform(state: String): Statement
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType
}

case class Assignment(lvalue: String, rvalue: Expr) extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-ASSIGN
    assert(env.typeMap(lvalue).isInstanceOf[SimpleType])
    val lvalueType = env.typeMap(lvalue).asInstanceOf[SimpleType]
    if (kappa.isConnected(rvalue.caissonType(env, kappa).level, lvalueType.level)) CommandType(lvalueType)
    else throw new CaissonTypeException("Cannot perform assignment to "+lvalue+": Incompatible value on right hand side")
  }

  def fallTransform(state: String) = this
}

case class Branch(cond: Expr, thenBody: Command, elseBody: Option[Command]) extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-IF
    val condType = cond.caissonType(env, kappa)
    val thenBodyType = thenBody.caissonType(env, kappa).level
    val elseBodyType = elseBody match {
      case Some(x) => x.caissonType(env, kappa).level
      case None => thenBodyType
    }
    val bodyType = TypeUtil.meet(kappa, thenBodyType, elseBodyType)
    if (kappa.isConnected(condType.level, bodyType.level)) CommandType(bodyType)
    else throw new CaissonTypeException("Illegal assignments in higher contexts")
  }

  def fallTransform(state: String) = {
    val transformedElse = elseBody match {
      case Some(c) => Some(c.fallTransform(state))
      case None => None
    }
    Branch(cond, thenBody.fallTransform(state), transformedElse)
  }
}

case class Jump(target: String, argList: List[String]) extends Statement {
  def fallTransform(state: String) = this

  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-GOTO
    assert(env.typeMap(target).isInstanceOf[StateType])
    val targetStateType = env.typeMap(target).asInstanceOf[StateType]
    if (targetStateType.paramTypeList.length != argList.length) throw new InvalidProgramException("Mismatch in arguments and parameters in goto "+target)
    val typeSubstitutionMap = List.range(0, argList.length).foldLeft(Map[String, SimpleType]())((m, i) => m ++ Map(targetStateType.paramTypeList(i).level -> env.typeMap(argList(i)).asInstanceOf[SimpleType]))
    val sourceType = Util.substituteType(targetStateType.level, typeSubstitutionMap)
    val substitutedConstraints = Util.substituteConstraints(targetStateType.constraints, typeSubstitutionMap)
    if (substitutedConstraints.forall((x: (SimpleType, SimpleType)) => kappa.isConnected(x._1.level, x._2.level))) CommandType(sourceType)
    else throw new CaissonTypeException("Illegal goto")
  }
}

case class Fall(level: Option[String]) extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-FALL
    assert(env.typeMap(extractLevel).isInstanceOf[StateType])
    val stateLevel = env.typeMap(extractLevel).asInstanceOf[StateType].level
    val defaultChildCommandLevel = env.functions.command(env.functions.default(extractLevel)).caissonType(env, kappa).level
    if (kappa.isConnected(stateLevel.level, defaultChildCommandLevel.level)) CommandType(stateLevel)
    else throw new CaissonTypeException("Illegal fall")
  }

  private def extractLevel: String = level match {
    case Some(x) => x
    case None => println("Internal compiler error: Fall not labelled"); exit(-1)
  }

  def fallTransform(state: String) = Fall(Some(state))
}

case class Skip() extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = CommandType(SimpleType("H")) //implements T-SKIP

  def fallTransform(state: String) = this
}

sealed abstract class Definition extends CaissonASTNode {
  def computeEnvironment(state: String): Environment
  def fallTransform(state: String): Definition
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType
  def validateAndGetNames: Set[String]
  def validateTypeVars(validTypeVarSet: Set[String]): Set[String]
}

case class LetDefinition(stateDefList: List[StateDefinition], cmd: Command)  extends Definition {
  def computeEnvironment(state: String): Environment = {
    val fcmd = Map(state -> cmd)
    val fdef = Map(state -> stateDefList(0).label)
    stateDefList.map(_.computeEnvironment(state)).reduceLeft((a, b) => a+b) + new Environment(Map(), new FunctionMapping(fcmd, fdef))
  }

  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-DEF
    val stateTypes = stateDefList.map(_.caissonType(env, kappa))
    discard(stateTypes)
    cmd.caissonType(env, kappa)
  }

  def fallTransform(state: String) = LetDefinition(stateDefList.map(_.fallTransform(state)), cmd.fallTransform(state))

  private def discard(x: Any) {}

  def validateAndGetNames: Set[String] = {
    stateDefList.foldLeft(Set[String]())((s: Set[String], e: StateDefinition) => {
      val eNames = e.validateAndGetNames
      if ((s ** eNames).isEmpty) s ++ eNames
      else throw new InvalidProgramException("Repeated names in state definitions")
    })
  }

  def validateTypeVars(validTypeVarSet: Set[String]): Set[String] = {
    stateDefList.foldLeft(Set.empty[String])((s: Set[String], e: StateDefinition) => {
      val eTypeVars = e.validateTypeVars(validTypeVarSet)
      if ((s ** eTypeVars).isEmpty) s ++  eTypeVars
      else throw new InvalidProgramException("Repeated type variable names in state definitions")
    })
  }
}

case class Command(stmtList: List[Statement]) extends Definition {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-SEQ
    CommandType(stmtList.foldLeft(SimpleType("H"))((t: SimpleType, s: Statement) => TypeUtil.meet(kappa, t, s.caissonType(env, kappa).level)))
  }

  def computeEnvironment(state: String): Environment = new Environment(Map.empty[String, CaissonType], new FunctionMapping(Map(state -> this), Map()))

  def fallTransform(state: String) = Command(stmtList.map(_.fallTransform(state)))

  def validateAndGetNames: Set[String] = Set.empty[String]

  def validateTypeVars(validTypeVarsSet: Set[String]): Set[String] = Set.empty[String]
}

class StateDefinition(name: String, secLevel: String, paramAndTypeList: List[(String, String)], constraintList: Option[List[(String,String)]], definition: Definition) {
  def label = name

  def computeEnvironment(state: String): Environment = {
    val constraints = constraintList match {
      case Some(x) => x.map(a => (SimpleType(a._1), SimpleType(a._2)))
      case None => List[Tuple2[SimpleType, SimpleType]]()
    }
    val stateNameMapping = Map(name -> StateType(SimpleType(secLevel), paramAndTypeList.map((x: (String, String)) => SimpleType(x._2)), constraints))
    val stateParamMapping = paramAndTypeList.foldLeft(Map[String, CaissonType]())((m, t) => m ++ Map(t._1 -> SimpleType(t._2)))
    (new Environment(stateNameMapping ++ stateParamMapping, new FunctionMapping(Map(), Map()))) + definition.computeEnvironment(name)
  }

  def caissonType(env: Environment, kappa: DirectedLatticeGraph): StateType = { //implements T-STATE
    assert(env.typeMap(name).isInstanceOf[StateType])
    val stateType = env.typeMap(name).asInstanceOf[StateType]
    val defnType = definition.caissonType(env, kappa)
    if (kappa.isConnected(stateType.level.level, defnType.level.level)) stateType
    else throw new CaissonTypeException("State type mismatch: "+stateType.level.level+" does not match "+defnType.level.level+" in state "+name)
  }

  def fallTransform(state: String) = new StateDefinition(name, secLevel, paramAndTypeList, constraintList, definition.fallTransform(name))

  def validateAndGetNames: Set[String] = {
    val stateNames = paramAndTypeList.foldLeft(Set(name))((s: Set[String], e: (String, String)) => {
      if (s.contains(e._1)) throw new InvalidProgramException("Reused name: "+ e._1)
      else s ++ Set(e._1)
    })
    val definitionNames = definition.validateAndGetNames
    if ((stateNames ** definitionNames).isEmpty) stateNames ++ definitionNames
    else throw new InvalidProgramException("Reused names in parent-child states")
  }

  def validateTypeVars(validTypeVarsSet: Set[String]): Set[String] = {
    val typeVarsSet = Set.empty[String] ++ paramAndTypeList.map(_._2)
    if(! (typeVarsSet ** validTypeVarsSet).isEmpty) throw new InvalidProgramException("State "+name+" has invalid security level(s)")
    if(! (typeVarsSet ++ validTypeVarsSet).contains(secLevel)) throw new InvalidProgramException("State "+name+" has invalid security level: "+secLevel)
    definition.validateTypeVars(validTypeVarsSet ++ typeVarsSet) ++ typeVarsSet
  }
}

sealed abstract class DataType
case class Input() extends DataType
case class Output() extends DataType
case class Register() extends DataType
case class Inout() extends DataType
case class Imem() extends DataType
case class Dmem() extends DataType
case class Wire() extends DataType

class DataStructure(dType: DataType, dimension: Option[Tuple2[Int, Int]])

class DataDeclaration(dStructure: DataStructure, name: String, level: String) {
  def computeEnvironment(state: String): Environment = new Environment(Map(name -> SimpleType(level)), new FunctionMapping(Map(), Map()))

  def getName = name

  def getLevel = level
}

class Program(name: String, params: List[String], decl: List[DataDeclaration], defn: Definition) {
  def computeEnvironment: Environment = decl.map(_.computeEnvironment(name)).reduceLeft((a, b) => a+b) + defn.computeEnvironment(name) + (new Environment(Map(name -> Util.bottomStateType), Util.emptyFunctionMapping))

  def fallTransform = new Program(name, params, decl, defn.fallTransform(name))

  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = defn.caissonType(env, kappa) //implements T-PROG

  def validateNames: Set[String] = {
    val usedNamesInDeclaration = decl.map(_.getName).foldLeft(Set[String]())((s: Set[String], e: String) => {
      if (s.contains(e)) throw new InvalidProgramException("Reused name: "+e)
        else s ++ Set(e)})
    val usedNamesInDefinition = defn.validateAndGetNames
    val usedNamesSet = if ((usedNamesInDeclaration ** usedNamesInDefinition).isEmpty) { usedNamesInDeclaration ++ usedNamesInDefinition } else throw new InvalidProgramException("Definition redeclares names in Declaration")
    if (usedNamesSet.contains(name)) throw new InvalidProgramException("Reused name: "+name)
    else usedNamesSet ++ Set(name)
  }

  def validateTypeVars(validTypeVarSet: Set[String]): Set[String] = {
    if (! decl.forall(dec => validTypeVarSet.contains(dec.getLevel))) throw new InvalidProgramException("Data Declarations use invalid security type")
    defn.validateTypeVars(validTypeVarSet)
  }
}

class InvalidProgramException(msg: String) extends Exception{
  def message = msg
}
