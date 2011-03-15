object Util {
  def emptyConstraintList: ConstraintList = List[(SimpleType, SimpleType)]()

  def substituteConstraints(cl: ConstraintList, subs: Map[String, SimpleType]): ConstraintList = {
    cl.map(e => (substituteType(e._1, subs), substituteType(e._2, subs)))
  }

  def substituteType(t: SimpleType, m: Map[String, SimpleType]) = {
    if (m.contains(t.level)) m(t.level)
    else t
  }

}

