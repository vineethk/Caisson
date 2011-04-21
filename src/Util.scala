object Util {
  def substituteConstraints(cl: List[(SimpleType, SimpleType)], subs: Map[String, SimpleType]): List[(SimpleType, SimpleType)] = {
    cl.map(e => (substituteType(e._1, subs), substituteType(e._2, subs)))
  }

  def substituteType(t: SimpleType, m: Map[String, SimpleType]): SimpleType = {
    if (m.contains(t.level)) m(t.level)
    else t
  }
  
  def bottomStateType = new StateType(SimpleType("L"), List[SimpleType](), List[(SimpleType, SimpleType)]())
  
  def emptyFunctionMapping = new FunctionMapping(Map(), Map())
  
  def computeKappa(tm: Map[String, CaissonType]): DirectedLatticeGraph = {
    val constraints = tm.values.filter(_.isInstanceOf[StateType]).map(_.asInstanceOf[StateType].constraints).reduceLeft((a, b) => a ++ b)
    val kappa = new DirectedLatticeGraph()
    constraints.foreach(c => kappa.addEdge(c._1.level, c._2.level))
    if (kappa.isConsistent) kappa
    else throw new CaissonTypeException("Inconsistent type constraints specification")
  }  
}