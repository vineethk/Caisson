sealed abstract class CaissonType {
  def level: Any
}
case class SimpleType(lvl: String) extends CaissonType {
  def level: String = lvl
  override def hashCode = level.hashCode
  override def equals(other: Any) = other match {
    case that: SimpleType => this.level == that.level
    case _ => false
  }
}

class CompositeType(left: SimpleType, right: SimpleType, op: LatticeOperation) extends SimpleType {
  def level: String = left.level + (op match { case _: Join => "\\/"; case _: Meet => "/\\" } ) + right.level
}

case class CommandType(lvl: SimpleType) extends CaissonType {
  def level: SimpleType = lvl
  override def hashCode = level.hashCode
  override def equals(other: Any) = other match {
    case that: CommandType => this.level == that.level
    case _ => false
  }
}

case class StateType(lvl: SimpleType, ptl: List[SimpleType], cnstnts: List[Tuple2[SimpleType, SimpleType]]) extends CaissonType {
  def level: SimpleType = level
  def paramTypeList: List[SimpleType] = ptl
  def constraints: List[Tuple2[SimpleType, SimpleType]] = cnstnts
}

class CaissonTypeException(msg: String) extends Exception {
  def message: String = msg
}

object TypeUtil {
  def meet(left: SimpleType, right: SimpleType): CaissonType = {
    val highType = SimpleType("H")
    val lowType = SimpleType("L")
    if (left == lowType || right == lowType) lowType
    else if (left == highType) right
    else if (right == highType) left
    else new CompositeType(left, right, Meet())
  }

  def join(left: SimpleType, right: SimpleType): CaissonType = {
    val highType = SimpleType("H")
    val lowType = SimpleType("L")
    if (left == highType || right == highType) highType
    else if (left == lowType) right
    else if (right == lowType) left
    else new CompositeType(left, right, Join())
  }
}

