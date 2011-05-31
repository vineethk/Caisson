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

  def codeGen(rhsMap: Map[String, String]): String
}

case class Number(value: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = SimpleType("L") //implements rule T-CONST

  def codeGen(rhsMap: Map[String, String]): String = value
}

case class Variable(name: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = { //implements rule T-REG/VAR
    assert(env.typeMap(name).isInstanceOf[SimpleType])
    env.typeMap(name).asInstanceOf[SimpleType]
  }

  def codeGen(rhsMap: Map[String, String]): String = rhsMap(name)
}

case class ComplexExpr(left: Expr, right: Expr, op: String) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = TypeUtil.join(kappa, left.caissonType(env, kappa), right.caissonType(env, kappa)) //implements rule T-OP

  def codeGen(rhsMap: Map[String, String]): String = left.codeGen(rhsMap) + " " + op + " " + right.codeGen(rhsMap)
}

case class UnaryExpr(operator: String, operand: Expr) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = operand.caissonType(env, kappa)

  def codeGen(rhsMap: Map[String, String]): String = operator + operand.codeGen(rhsMap)
}

case class ArrayExpr(name: Variable, dimension1: Expr, dimension2: Option[Expr]) extends Expr {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): SimpleType = { //implements T-ARRAY
    val partialType = TypeUtil.join(kappa, name.caissonType(env, kappa), dimension1.caissonType(env, kappa))
    dimension2 match {
      case Some(x) => TypeUtil.join(kappa, partialType, x.caissonType(env, kappa))
      case None => partialType
    }
  }

  def codeGen(rhsMap: Map[String, String]): String = {
    name.codeGen(rhsMap) + "[" + dimension1.codeGen(rhsMap) + "]" + (dimension2 match {
      case Some(x) => "[" + x.codeGen(rhsMap) + "]"
      case None => ""
    })
  }
}

sealed abstract class Statement extends CaissonASTNode {
  def fallTransform(state: String): Statement
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType
  def computeGotoInformation: List[(String, List[String])]
  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String
  def validateGotos(allowedLabels: Set[String]) { } //default behaviour for all Statements: always pass the check (overriden for a few of Statement types: goto, if, case etc)
  def hasFallOrGoto(): Boolean = false
  def endsWithFallOrGoto(): Boolean = false
}

case class Assignment(lvalue: String, rvalue: Expr) extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-ASSIGN
    assert(env.typeMap(lvalue).isInstanceOf[SimpleType])
    val lvalueType = env.typeMap(lvalue).asInstanceOf[SimpleType]
    if (kappa.isConnected(rvalue.caissonType(env, kappa).level, lvalueType.level)) CommandType(lvalueType)
    else throw new CaissonTypeException("Cannot perform assignment to "+lvalue+": Incompatible value on right hand side")
  }

  def fallTransform(state: String) = this

  def computeGotoInformation: List[(String, List[String])] = {
    List.empty[(String, List[String])]
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    val lhs = lhsMap(lvalue)
    val rhs = rvalue.codeGen(rhsMap)
    if (lhs.length == 1) lhs(0) + " = " + rhs  + ";\n"
    else {
      "case (" + lhs(0) + ")\n" +
      List.range(0, (lhs.length-1)).map((i: Int) => {
        i + ": begin\n" +
        lhs(i+1) + " = " + rhs + ";\n" +
        "end\n"
      }).mkString +
      "endcase\n"
    }
  }
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

  //gotoInformation will be the one in thenbody + the one in elsebody
  def computeGotoInformation: List[(String, List[String])] = {
    thenBody.computeGotoInformation ++ (elseBody match {
      case Some(x) => x.computeGotoInformation
      case None => List.empty[(String, List[String])]
    })
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    "if (" + cond.codeGen(rhsMap) + ") begin\n" +
    thenBody.codeGen(leafState, e, stateInformation, curStateNode, curLevel, lhsMap, rhsMap) +
    "end\n" +
    (elseBody match {
      case Some(x) => "else begin\n" + x.codeGen(leafState, e, stateInformation, curStateNode, curLevel, lhsMap, rhsMap) + "end\n"
      case None => ""
    })
  }

  override def validateGotos(allowedLabels: Set[String]) {
    //check the else and then branches for goto labels in the allowed set
    thenBody.validateGotos(allowedLabels)
    elseBody match {
      case Some(x) => x.validateGotos(allowedLabels)
      case _ => ; //if there is no else, dont do anything
    }
  }

  override def hasFallOrGoto() = {
    thenBody.hasFallOrGoto || ( elseBody match {
      case Some(x) => x.hasFallOrGoto()
      case None => false
    } )
  }

  override def endsWithFallOrGoto(): Boolean = {
    thenBody.endsWithFallOrGoto() && ( elseBody match {
      case Some(x) => x.endsWithFallOrGoto()
      case None => false
    } )
  }
}

case class Kase(cond: Expr, caseMap: Map[List[String], Command]) extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = { //implements T-CASE
    val condType = cond.caissonType(env, kappa)
    val bodyTypeList = caseMap.values.toList.map(_.caissonType(env, kappa).level)
    val bodyTypeLevel = bodyTypeList.foldLeft(SimpleType("H"))((t1: SimpleType, t2: SimpleType) => TypeUtil.meet(kappa, t1, t2))
    if(kappa.isConnected(condType.level, bodyTypeLevel.level)) CommandType(bodyTypeLevel)
    else throw new CaissonTypeException("Case statements not typeable")
  }
  
  def fallTransform(state: String) = {
    Kase(cond, caseMap.toList.map((c) => Map(c._1 -> c._2.fallTransform(state))).reduceLeft((a, b) => a ++ b))
  }

  //find the gotoInformation of each of the bodies and add them up
  def computeGotoInformation: List[(String, List[String])] = {
    caseMap.values.toList.foldLeft(List.empty[(String, List[String])])((a: List[(String, List[String])], b: Command) => {
      a ++ b.computeGotoInformation
    })
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    "case (" + cond.codeGen(rhsMap) + ")\n" +
    caseMap.keys.toList.map((k: List[String]) => {
      k.mkString(", ") + ": begin\n" +
      caseMap(k).codeGen(leafState, e, stateInformation, curStateNode, curLevel, lhsMap, rhsMap) +
      "end\n"
    })
    "endcase\n"
  }

  override def validateGotos(allowedLabels: Set[String]) {
    //go through each of the case commands and check if they have valid gotos
    caseMap.values.foreach(_.validateGotos(allowedLabels))
  }

  override def hasFallOrGoto() = {
    caseMap.values.exists(_.hasFallOrGoto())
  }

  override def endsWithFallOrGoto(): Boolean = {
    caseMap.values.forall((c: Command) => c.endsWithFallOrGoto())
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

  def computeGotoInformation: List[(String, List[String])] = {
    if (argList.length > 0) List((target, argList))
    else List.empty[(String, List[String])]

  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    "cur_state_wout = " + stateInformation(target).defaultLeafID + ";\n" +
    (if (argList.length > 0) {
     (stateInformation(target).getGotoInfo match {
       case Some(l) =>  l._2 + " = " + l._1.indexOf(argList) + ";\n"
       case None => throw new CaissonCompilerException("Error in generating Jump code")
    })}
    else "")
  }

  override def validateGotos(allowedLabels: Set[String]) {
    if (! allowedLabels.contains(target)) throw new ValidationException("Invalid goto, state "+target+" is not at the same level and group" )
  }

  override def hasFallOrGoto() = true

  override def endsWithFallOrGoto() = true

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

  def computeGotoInformation: List[(String, List[String])] = {
    List.empty[(String, List[String])]
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    /*val correspondingState = level match {
      case Some(x) => x
      case None => throw new CaissonCompilerException("Error constructing Fall labels")
    }*/
    val pathIndex = stateInformation(leafState).getPathFromRoot match {
      case Some(x) => x(curLevel)
      case None => throw new CaissonCompilerException("Error constructing pathFromRoot information")
    }
    val fallNode = curStateNode.getChildren match {
      case Some(x) => x(pathIndex)
      case None => throw new CaissonCompilerException("Error constructing StateNodes")
    }
    val gotoInfo = stateInformation(fallNode.getName).getGotoInfo
    (gotoInfo match {
      case Some(x) =>  {
        val gotoInfoList = x._1
        val assignmentIndices = x._3.keys.toList
        val additionalRhsMap = assignmentIndices.foldLeft(Map[String, String]())((a: Map[String, String], b: Int) => {
          a ++ Map(fallNode.getParams(b) -> x._3(b))})
        val additionalLhsMap = List.range(0, fallNode.getParams.length).foldLeft(Map[String, List[String]]())((a: Map[String, List[String]], b: Int) => {
          a ++
          Map(fallNode.getParams(b) -> (List(x._2) ++ gotoInfoList.map(_(b) + "_wout")))
        })
        "case (" + x._2 + ")\n" +
        List.range(0, gotoInfoList.length).map((i: Int) => {
          i + ": begin\n" +
          assignmentIndices.map((j: Int) => {
            x._3(j) + " = " + rhsMap(gotoInfoList(i)(j)) + ";\n"
          }).mkString +
          "end\n"
        }).mkString +
        "endcase\n" +
        e.functions.command(fallNode.getName).codeGen(leafState, e, stateInformation, fallNode, curLevel + 1, lhsMap ++ additionalLhsMap, rhsMap ++ additionalRhsMap)
      }
      case None => e.functions.command(fallNode.getName).codeGen(leafState, e, stateInformation, fallNode, curLevel + 1, lhsMap, rhsMap)
    })
   }

  override def hasFallOrGoto() = true

  override def endsWithFallOrGoto() = true
}

case class Skip() extends Statement {
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType = CommandType(SimpleType("H")) //implements T-SKIP

  def fallTransform(state: String) = this

  def computeGotoInformation: List[(String, List[String])] = {
    List.empty[(String, List[String])]
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    ";\n"
  }
}

sealed abstract class Definition extends CaissonASTNode {
  def computeEnvironment(state: String): Environment
  def fallTransform(state: String): Definition
  def caissonType(env: Environment, kappa: DirectedLatticeGraph): CommandType
  def validateAndGetNames: Set[String]
  def validateTypeVars(validTypeVarSet: Set[String]): Set[String]
  def computePartialStateInfo(pathFromRoot: List[Int]): Map[String, StateInfo]
  def computeGotoInformation: List[(String, List[String])]
  def extractStateNodeStructure: Option[List[StateNode]]
  def validateDefaultStateAssumption(): Unit
  def validateGotos(allowedLabels: Set[String]): Unit
  def validateFallsAndGotos(): Unit
}

case class LetDefinition(stateDefList: List[StateDefinition], cmd: Command)  extends Definition {
  def getStateDefList = stateDefList

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
      if ((s & eNames).isEmpty) s ++ eNames
      else throw new InvalidProgramException("Repeated names in state definitions")
    })
  }

  def validateTypeVars(validTypeVarSet: Set[String]): Set[String] = {
    stateDefList.foldLeft(Set.empty[String])((s: Set[String], e: StateDefinition) => {
      val eTypeVars = e.validateTypeVars(validTypeVarSet)
      if ((s & eTypeVars).isEmpty) s ++  eTypeVars
      else throw new InvalidProgramException("Repeated type variable names in state definitions")
    })
  }

  def computePartialStateInfo(pathFromRoot: List[Int]): Map[String, StateInfo] = {
    val numberOfStates = stateDefList.length
    List.range(0, numberOfStates).foldLeft(Map.empty[String, StateInfo])(
    (a: Map[String, StateInfo], b: Int) => a ++ stateDefList(b).computePartialStateInfo(pathFromRoot ++ List(b)))
  }

  def computeGotoInformation: List[(String, List[String])] = {
    stateDefList.foldLeft(List.empty[(String, List[String])])((a: List[(String, List[String])], b: StateDefinition) => {
      a ++ b.computeGotoInformation
    }) ++ cmd.computeGotoInformation
  }

  def extractStateNodeStructure: Option[List[StateNode]] = {
    Some(stateDefList.map(_.extractStateNodeStructure))
  }

  def validateDefaultStateAssumption() {
    if (stateDefList.length < 1) throw new ValidationException("let definition does not define anything")
    //check that the default state does not take any parameters
    if (!stateDefList(0).doesNotTakeParams) throw new ValidationException("let definition does not define anything")
    stateDefList.foreach(_.validateDefaultStateAssumption()) //check this recursively on all defined states
  }

  def validateGotos(allowedLabels: Set[String]) {
    cmd.validateGotos(allowedLabels) //first check the command for correct gotos
    //then check each of the state definitions for correct gotos, assuming the new set of allowed labels
    val collectedLabels = stateDefList.map(_.label).toSet
    stateDefList.foreach(_.validateGotos(collectedLabels))
  }

  def validateFallsAndGotos() {
    stateDefList.foreach(_.validateFallsAndGotos()) //check if the all statements have valid falls and gotos
    cmd.validateFallsAndGotos() //check if the command has well formed falls and gotos
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

  def countLeafStates: Int = 1

  def computePartialStateInfo(pathFromRoot: List[Int]): Map[String, StateInfo] = Map.empty[String, StateInfo]

  def computeGotoInformation: List[(String, List[String])] = {
    stmtList.foldLeft(List.empty[(String, List[String])])((a: List[(String, List[String])], b: Statement) => {
      a ++ b.computeGotoInformation
    })
  }

  def extractStateNodeStructure: Option[List[StateNode]] = None

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode, curLevel: Int, lhsMap: Map[String, List[String]], rhsMap: Map[String, String]): String = {
    stmtList.map(_.codeGen(leafState, e, stateInformation, curStateNode, curLevel, lhsMap, rhsMap)).mkString
  }

  def validateDefaultStateAssumption() { } // always valid (hence cannot throw an Exception), because it cannot contain a default state definition

  def validateGotos(allowedLabels: Set[String]) {
    stmtList.last.validateGotos(allowedLabels)
  }

  def validateFallsAndGotos() {
    //make sure there is at least one statement
    if (stmtList.length < 1) throw new ValidationException("A command should contain at least one statement")
    //1. make sure none except the last Statement in the list have goto or fall
    if (stmtList.init.exists(_.hasFallOrGoto)) throw new ValidationException("Fall or goto can only be used as the last statement in a path, deadcode found")
    //2. make sure the last Statement has either a goto or a fall
    if (!stmtList.last.endsWithFallOrGoto) throw new ValidationException("All paths must end with fall or goto: "+stmtList.last)
  }

  def hasFallOrGoto(): Boolean = {
    stmtList.exists(_.hasFallOrGoto()) //has a goto or a fall if any of its constituent statements do
  }

  def endsWithFallOrGoto(): Boolean = {
    stmtList.last.endsWithFallOrGoto() //ends with a fall or goto if the last statement ends with fall or goto
  }
}

class StateDefinition(name: String, secLevel: String, paramAndTypeList: List[(String, String)], constraintList: Option[List[(String,String)]], definition: Definition) {
  def label = name

  def getDefinition = definition

  def doesNotTakeParams: Boolean = (paramAndTypeList.length == 0)

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

  def computePartialStateInfo(pathFromRoot: List[Int]): Map[String, StateInfo] = {
    val childInfo = definition.computePartialStateInfo(pathFromRoot)
    val isLeaf = definition.isInstanceOf[Command]
    val defLeafID = if (isLeaf) Util.leafStateIDGenerator else Util.getDefaultLeafID(definition.asInstanceOf[LetDefinition], childInfo)

    Map(name -> new StateInfo(isLeaf, defLeafID, pathFromRoot)) ++ childInfo
  }

  def computeGotoInformation: List[(String, List[String])] = {
    definition.computeGotoInformation
  }

  def extractStateNodeStructure: StateNode = {
    new StateNode(name, definition.extractStateNodeStructure, paramAndTypeList.map(_._1))
  }

  def validateDefaultStateAssumption() {
    definition.validateDefaultStateAssumption()
  }

  def validateGotos(allowedLabels: Set[String]) {
    definition.validateGotos(allowedLabels)
  }

  def validateFallsAndGotos() {
    definition.validateFallsAndGotos()
  }
}

sealed abstract class DataType {
  def genCode: String
}
case class Input() extends DataType {
  def genCode = "input"
}
case class Output() extends DataType {
  def genCode = "output"
}
case class Register() extends DataType {
  def genCode = "reg"
}
case class Inout() extends DataType {
  def genCode = "inout"
}
case class Imem() extends DataType {
  def genCode = "imem"
}
case class Dmem() extends DataType {
  def genCode = "dmem"
}
case class Wire() extends DataType {
  def genCode = "wire"
}

class DataStructure(dType: DataType, dimension1: Option[(Int, Int)], dimension2: Option[(Int, Int)]) {
  def this(ds: DataStructure, dim2: Option[(Int, Int)]) = this(ds.dataType, ds.dim1, dim2)
  
  def dataType = dType
  
  def dim1 = dimension1

  def dim2 = dimension2

  def genCode = dType.genCode + genDimensionCode

  def genDimensionCode = {
    (dimension1 match {
      case Some(x) => "[" + x._1 + ":" + x._2 +"]"
      case None => ""
    }) + (
    dimension2 match {
      case Some(x) => "[" + x._1 + ":" + x._2 +"]"
      case None => ""
    })
  }
}

class DataDeclaration(dStructure: DataStructure, name: String, level: String) {
  def computeEnvironment(state: String): Environment = new Environment(Map(name -> SimpleType(level)), new FunctionMapping(Map(), Map()))

  def getName = name

  def getLevel = level

  def getDataStructure = dStructure

  def genCode: String = {
    dStructure.genCode + " " + name + ";\n" +
    (dStructure.dataType match {
      case d: Input => ""
      case d: Output => {
        "reg" + dStructure.genDimensionCode + " " + name + "_wout;\n"
      }
      case d: Register => { "wire" + dStructure.genDimensionCode + " " + name + "_win;\n" +
        "reg" + dStructure.genDimensionCode + " " + name + "_wout;\n"
      }
      case d: Inout => ""
      case d: Imem => ""
      case d: Dmem => ""
      case d: Wire => ""
    })
  }
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
    val usedNamesSet = if ((usedNamesInDeclaration & usedNamesInDefinition).isEmpty) { usedNamesInDeclaration ++ usedNamesInDefinition } else throw new InvalidProgramException("Definition redeclares names in Declaration")
    if (usedNamesSet.contains(name)) throw new InvalidProgramException("Reused name: "+name)
    else usedNamesSet ++ Set(name)
  }

  def validateTypeVars(validTypeVarSet: Set[String]): Set[String] = {
    if (! decl.forall(dec => validTypeVarSet.contains(dec.getLevel))) throw new InvalidProgramException("Data Declarations use invalid security type")
    defn.validateTypeVars(validTypeVarSet)
  }

  def codeGen(e: Environment): String = {
    val regNames = decl.filter((x: DataDeclaration) => x.getDataStructure.dataType.isInstanceOf[Register]).map(_.getName) ++ List("cur_state")
    val outNames = decl.filter((x: DataDeclaration) => x.getDataStructure.dataType.isInstanceOf[Output]).map(_.getName)
    val stateInformation = stateInfoMap
    val leafStates = stateInfoMap.keys.toList.filter(stateInfoMap(_).isLeaf)
    val numberOfLeafStates = leafStates.length
    val stateNode = extractStateNodeStructure
    val leafStateCode = leafStates.map(codeGen(_, e, stateInformation, stateNode)).mkString
    val curStateDimension = "[" + (Util.bitsForRepresenting(numberOfLeafStates) - 1) + ":0]"

    "module " + name + "(" + (params ++ List("clk", "reset")).mkString(",") + ");\n" +
    "input clk; \n" +
    "input reset; \n" +
    "reg" + curStateDimension + " cur_state;\n" +
    "wire" + curStateDimension + " cur_state_win;\n" +
    "reg" + curStateDimension + " cur_state_wout;\n" +
    decl.map((x: DataDeclaration) => x.genCode).mkString +
    genTempVariableDeclarations(stateInfoMap.values.toList) +
    outNames.map((x: String) => "assign " + x + " = " + x + "_wout;\n").mkString +
    regNames.map((x: String) => "assign " + x + "_win = " + x + ";\n").mkString +
    "\nalways @ (posedge clk)\nbegin\n" +
    regNames.map((x: String) => x + " <= " + x + "_wout;\n").mkString +
    "end\n\n" +
    "always @ (*)\nbegin\n" +
    regNames.map((x: String) => x + "_wout = " + x + "_win;\n").mkString +
    "if (reset) begin\n" +
    regNames.map((x: String) => x + "_wout = 0;\n").mkString +
    "end\n" +
    "else begin\n" +
    "case (cur_state_win)\n" +
    leafStateCode +
    "default: ;\n" +
    "endcase\n" + // end for leaf state cases
    "end\n" + //end else
    "end\n" + //end begin
    "endmodule"
  }

  def stateInfoMap: Map[String, StateInfo] =  {
    val partialStateInfo = defn.computePartialStateInfo(List.empty[Int]) //get isLeaf, defaultLeafID and pathRoot information
    val gotoInformation = defn.computeGotoInformation
    val states = partialStateInfo.keys.toList
    states.foldLeft(Map.empty[String, StateInfo])((a: Map[String, StateInfo], b: String) => {
      val stateGotoInfo = gotoInformation.filter((x: (String, List[String])) => x._1 == b).map(_._2)
      val index = states.findIndexOf((s: String) => s == b)
      a ++ Map(b -> new StateInfo(partialStateInfo(b),
        if (stateGotoInfo.length > 0)
          Some((stateGotoInfo, "condition"+index, List.range(0, stateGotoInfo(0).length).foldLeft(Map[Int, String]())((m: Map[Int, String], i: Int) => {
            m ++
            (if (decl.exists((d: DataDeclaration) => (d.getName == stateGotoInfo(0)(i)) &&
            (d.getDataStructure.dataType match {
              case dt: Register => true
              case dt: Input => true
              case _ => false
            })))
              Map(i -> (stateGotoInfo.map(_(i)).mkString("_") + "_" + b + "_w"))
            else
              Map[Int, String]())
          })))
        else None))
    })
  }

  def extractStateNodeStructure: StateNode = { //model state structure
    new StateNode(name, defn.extractStateNodeStructure, List[String]())
  }

  def codeGen(leafState: String, e: Environment, stateInformation: Map[String, StateInfo],  curStateNode: StateNode): String = {
    val lhsMap = decl.foldLeft(Map[String, List[String]]())((m: Map[String, List[String]], d: DataDeclaration) => {
      m ++ (d.getDataStructure.dataType match {
        case dt: Register => Map(d.getName -> List(d.getName + "_wout"))
        case dt: Output => Map(d.getName -> List(d.getName + "_wout"))
        case _ => Map(d.getName -> List(d.getName))
      })
    })
    val rhsMap = decl.foldLeft(Map[String, String]())((m: Map[String, String], d: DataDeclaration) => {
      m ++ (d.getDataStructure.dataType match {
        case dt: Register => Map(d.getName -> (d.getName + "_win"))
        case _ => Map(d.getName -> d.getName)
      })
    })
    //val rhsMap
    stateInformation(leafState).defaultLeafID + ":" + "begin\n" +
    e.functions.command(name).codeGen(leafState, e, stateInformation, curStateNode, 0, lhsMap, rhsMap) +
    "end\n"
  }

  def genTempVariableDeclarations(stateInfoList: List[StateInfo]): String = {
    stateInfoList.map((s: StateInfo) => {
      val gotoInfo = s.getGotoInfo
      gotoInfo match {
        case Some(x) => ("reg " + x._2 + ";\n" +
          x._3.keys.toList.map((i: Int) => {
            decl.find((d: DataDeclaration) => d.getName == x._1(0)(i)) match {
              case Some(e) => "reg" + e.getDataStructure.genDimensionCode + " " + x._3(i) + ";\n"
              case None => throw new CaissonCompilerException("Internal Compiler Error: Extra data hanging in compiler structures")
            }
          }).mkString)
        case None => ""
      }
    }).mkString
  }

  def validateDefaultStateAssumption() {
    defn.validateDefaultStateAssumption()
  }

  def validateGotos() {
    defn.validateGotos(Set.empty[String])
  }

  def validateFallsAndGotos() {
    defn.validateFallsAndGotos()
  }
}

class InvalidProgramException(msg: String) extends Exception{
  def message = msg
}

class StateNode(name: String, children: Option[List[StateNode]], params: List[String]) {
  def getName = name

  def getParams = params

  def getChildren = children
}

//Internal data structure for the compiler's code generator
class StateInfo(izLeaf: Boolean, defLeafID: Int, pathFromRoot: Option[List[Int]], gotoInfo: Option[(List[List[String]], String, Map[Int, String])]) {
  def this(a: Boolean, b: Int, c: List[Int]) = this(a, b, Some(c), None) //helper constructor

  def this(a: StateInfo, b: Option[(List[List[String]], String, Map[Int, String])]) = this(a.isLeaf, a.defaultLeafID, a.getPathFromRoot, b) //helper constructor

  def isLeaf = izLeaf //is a leaf state?

  def defaultLeafID = defLeafID //ID of the leaf state when you "goto" this state

  def getPathFromRoot = pathFromRoot

  def getGotoInfo = gotoInfo //Information from all goto sites to this state
}

class CaissonCompilerException(msg: String) extends Exception {
  def message: String = msg
}

class ValidationException(msg: String) extends Exception {
  def message: String = msg
}
/*class VariableInformation(registerS: Set[String], inputS: Set[String], outputS: Set[String], wireS: Set[String], inoutS: Set[String], imemS: Set[String], dmemS: Set[String]) {
  def registers = registerS

  def inputs = inputS

  def outputs = outputS

  def wires = wireS

  def inouts = inoutS

  def imems = imemS

  def dmems = dmemS
}*/