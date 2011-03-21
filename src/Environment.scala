class Environment(tm: Map[String, CaissonType], fm: FunctionMapping) {
  def typeMap: Map[String, CaissonType]  = tm
  def functions: FunctionMapping = fm
  def + (that: Environment): Environment = new Environment(tm ++ that.typeMap, fm + that.functions)
  
  override def toString = tm.toString
}

class FunctionMapping(cmd: Map[String, Command], dflt: Map[String, String]) {
  def command = cmd
  def default = dflt

  def + (that: FunctionMapping): FunctionMapping = new FunctionMapping(cmd ++ that.command, dflt ++ that.default)
}

